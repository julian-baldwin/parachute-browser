(in-package #:parachute-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Image Registration

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun load-external-image (path)
    "loads the external image at PATH and registers it with the file name as a keyword"
    (gp:register-image-translation (intern (string-upcase (pathname-name path)) :keyword)
                                   (gp:read-external-image path))))

(mapc #'load-external-image (directory (current-pathname "img/*.png")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Execution

(defun execute-tests-in-background-func (tests result-process)
  "background test execution function"
  (mp:process-send result-process
                   (list 'find-interface
                         'result-viewer
                         :report
                         (parachute:test tests :report 'parachute:quiet))))

(defun execute-tests-in-background (tests
                                    &key (result-process #-:cocoa (mp:get-current-process)
                                                         #+:cocoa mp:*main-process*))
  "execute list of test designators, delivering report to a RESULT-VIEWER in RESULT-PROCESS"
  (mp:process-run-function "parachute background tests"
                             '(:internal-server t)
                             'execute-tests-in-background-func
                             tests
                             result-process))

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
;;; Convenience Functions

(defun browse-tests ()
  "open a parachute test browser"
  (find-interface 'test-browser))
