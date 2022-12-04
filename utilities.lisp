(in-package #:parachute-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image Registration

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun load-external-image (path)
    "loads the external image at PATH and registers it with the file name as a keyword"
    (gp:register-image-translation (intern (string-upcase (pathname-name path)) :keyword)
                                   (gp:read-external-image path))))

(mapc 'load-external-image
      (directory (asdf:system-relative-pathname "parachute-browser" "img/*.png")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toolbar Support

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun sanitise-toolbar-item-args (callback args)
    "sanitises arguments to MAKE-INSTANCE used to create a toolbar item"
    (let ((args (copy-list args))) ;; copy to avoid mutating constant data
      (remf args :class)
      (setf (getf args :callback) (or (getf args :callback) callback)
            (getf args :callback-type) (or (getf args :callback-type) :interface-data)
            (getf args :data) (or (getf args :data) (getf args :name))
            (getf args :text) (or (getf args :text) (string-capitalize (getf args :name)))
            (getf args :tooltip) (or (getf args :tooltip) (getf args :text))
            (getf args :image) (or (getf args :image) (getf args :name)))
      args)))

(defmacro define-toolbar ((create-func callback) &body items)
  `(defun ,create-func ()
     (list ,@(loop for item in items
                   for class = (or (getf item :class) (quote 'toolbar-button))
                   collecting `(make-instance ,class
                                              ,@(sanitise-toolbar-item-args callback item))))))

(defmethod toolbar-item-enabled-p (interface item-name)
  "test if the toolbar ITEM-NAME should be enabled for INTERFACE"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base Interface

(define-interface parachute-window ()
  ()
  (:default-initargs
   :best-width 600
   :best-height 450
   :activate-callback (lambda (interface activep)
                        (declare (ignore activep))
                        (redisplay-interface interface))))

(defmethod initialize-instance :after ((interface parachute-window) &rest initargs)
  (declare (ignore initargs))
  ;; Hook up enabled functions for toolbar items where these are not
  ;; already set. Currying the item name allows dispatching on this
  ;; instead of just the interface itself which is how CAPI calls it.
  ;; Maybe a bit naughty to use unexported CAPI symbols
  (when-let (items (slot-value interface 'capi::toolbar-items))
    (when (listp items)
      (loop for item in items
            when (and (typep item 'toolbar-object) (null (toolbar-object-enabled-function item)))
            doing (setf (toolbar-object-enabled-function item)
                        (rcurry 'toolbar-item-enabled-p (capi-object-name item)))))))

(defmethod redisplay-interface :after ((interface parachute-window))
  (interface-update-toolbar interface))

(defmethod lispworks-tools::get-toolbar-items ((instance parachute-window))
  (slot-value instance 'capi::toolbar-items))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Execution

(defvar *automatic-refresh* nil "Automatically refresh test browser on definition.")
(defvar *automatic-execute* nil "Automatically execute tests on definition.")

(defun toggle-automatic-behaviour (&key (refresh (not *automatic-refresh*))
                                        (execute (not *automatic-execute*)))
  "Toggle the automatic behaviour flags."
  (list :refresh (setf *automatic-refresh* refresh)
        :execute (setf *automatic-execute* execute)))

(defun background-execute-results-handler (report automatic)
  "Handle results received from the background execution process."
  ;; If AUTOMATIC is true these tests were executed via automatic execution, therefore don't
  ;; focus the results window, to avoid having to click back to the editor after compiling.
  (when-let (results (or (locate-interface 'result-viewer :report report)
                         (and (not automatic)
                              (find-interface 'result-viewer :report report))))
    (unless automatic
      (raise-interface results))))

(defun execute-tests-in-background-func (tests result-process &key automatic debug)
  "background test execution function"
  (with-simple-restart (abort-all-tests "Abort all tests")
    (mp:process-send result-process
                     (list 'background-execute-results-handler
                           (parachute:test tests
                                           :report
                                           (if debug
                                               'parachute:interactive
                                             'parachute:quiet))
                           automatic))))

(defun execute-tests-in-background (tests
                                    &key (result-process #-:cocoa (mp:get-current-process)
                                                         #+:cocoa mp:*main-process*)
                                    automatic debug)
  "execute list of test designators, delivering report to a RESULT-VIEWER in RESULT-PROCESS"
  (mp:process-run-function "parachute background tests"
                           '(:internal-server t)
                           'execute-tests-in-background-func
                           tests
                           result-process
                           :automatic automatic
                           :debug debug))

(defun brief-summary (report)
  "generate a simple one line summary for a parachute report"
  (format nil "~d passed, ~d failed, ~d skipped"
          (length (parachute:results-with-status :passed report))
          (length (parachute:results-with-status :failed report))
          (length (parachute:results-with-status :skipped report))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tree Nodes

(defstruct result-node
  (parent nil)
  (children nil)
  (test nil)
  (status :unknown)
  (result nil))

;; A PRINT-OBJECT method is required to prevent default behaviour recursing unnecessarily,
;; resulting in a stack overflow.
(defmethod print-object ((node result-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "[~A] ~A" (result-node-status node) (result-node-test node))))

(defun build-result-tree (results)
  "convert the sequence of parachute test results into a tree of RESULT-NODE structs
based on the underlying test hierarchy"
  (let ((node-table (make-hash-table)))
    (labels ((node-for-test (test)
               (let ((node (ensure-gethash test node-table (make-result-node :test test))))
                 (when-let* ((parent-test (parachute:parent test))
                             (parent-node (node-for-test parent-test)))
                   (setf (result-node-parent node) parent-node)
                   (pushnew node (result-node-children parent-node)))
                 node))
             (process-result (result)
               (when (typep result 'parachute:test-result)
                 (let* ((test (parachute:expression result))
                        (node (node-for-test test)))
                   (setf (result-node-result node) result
                         (result-node-status node) (parachute:status result))
                   node)))
             (find-roots (nodes)
               (loop for node being the hash-values of nodes
                     when (and node (null (result-node-parent node)))
                       collect node)))
      (map nil #'process-result results)
      (values (find-roots node-table) node-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Source Navigation

(defgeneric find-source-for-item (datum)
  (:documentation "Attempt to open the source for DATUM in an editor.")
  (:method (datum) (capi:display-message "Don't know how to find source for ~A." datum))
  (:method ((datum null)) (values)))

(defmethod find-source-for-item ((datum package))
  (editor:find-source-command nil (package-name datum)))

(defmethod find-source-for-item ((datum parachute:test))
  (let ((dspec `(parachute-browser:define-test ,(parachute:name datum))))
    (if (dspec:find-dspec-locations dspec)
        (editor:find-source-for-dspec-command nil dspec)
        (display-message "Unable to find source for ~A. Check it was defined with Parachute Browser's DEFINE-TEST macro."
                         (parachute:name datum)))))

(dspec:define-dspec-class define-test nil "Defined a Parachute test.")

(defmacro define-test (name &body body)
  "Wrap PARACHUTE:DEFINE-TEST around a Dspec to allow source navigation with the LW editor."
  (let ((test-name (if (listp name) (second name) name)))
    `(eval-when (:load-toplevel :execute)
       (dspec:def (define-test ,test-name)
         (when (record-definition `(define-test ,',test-name) (dspec:location))
           (prog1
               (parachute:define-test ,name ,@body)
             (when-let (browser (and *automatic-refresh* (locate-interface 'test-browser)))
               (execute-with-interface-if-alive browser
                                                'browser-toolbar-callback
                                                browser
                                                :pb-refresh))
             (when *automatic-execute*
               (execute-tests-in-background ',test-name :automatic t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience Functions

(defun browse-tests ()
  "open a parachute test browser"
  (find-interface 'test-browser))

(defun load-examples ()
  "load the example tests shipped with Parachute Browser"
  (load (asdf:system-relative-pathname "parachute-browser" "example-tests.lisp"))
  (browse-tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Helpers

(defmacro pass-test (&rest reason)
  `(parachute:eval-in-context
    parachute:*context*
    (make-instance 'parachute:result
                   :expression 'explicitly-passed
                   :status :passed
                   :description (format nil ,@reason))))

(defmacro fail-test (&rest reason)
  `(parachute:eval-in-context
    parachute:*context*
    (make-instance 'parachute:result
                   :expression 'explicitly-failed
                   :status :failed
                   :description (format nil ,@reason))))
