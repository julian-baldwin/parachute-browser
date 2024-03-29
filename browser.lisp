(in-package #:parachute-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Browser

(define-interface test-browser (parachute-window lispworks-tools:lispworks-interface) ()
  (:panes
   (test-tree extended-selection-tree-view
              :reader browser-tree
              :roots (test-packages)
              :children-function 'browser-tree-children
              :print-function 'browser-tree-print
              :image-function 'browser-tree-image
              :alternating-background t
              :callback-type :interface
              :action-callback (rcurry 'browser-toolbar-callback :pb-run-tests)
              :alternative-action-callback (rcurry 'browser-toolbar-callback :pb-find-source)
              :selection-callback :redisplay-interface
              :retract-callback :redisplay-interface
              :interaction :extended-selection)
   (icons-copyright title-pane
                    :text "Icons by https://icons8.com/"
                    :font (gp:make-font-description :stock :system-font :size 10)))
  (:layouts
   (main-layout column-layout
                '(test-tree icons-copyright)
                :adjust :right
                :gap 5
                :internal-border 5))
  (:default-initargs
   :title "Parachute Test Browser"
   :toolbar-items (browser-toolbar)
   :default-toolbar-states
   '(:visible (:pb-run-tests :pb-debug :pb-results :pb-refresh :pb-find-source :pb-automatic))))

(define-toolbar (browser-toolbar 'browser-toolbar-callback)
  (:name :pb-run-tests :text "Run")
  (:name :pb-debug :text "Debug")
  (:name :pb-results :text "Results")
  (:name :pb-refresh :text "Refresh")
  (:name :pb-find-source :text "Source")
  (:name :pb-automatic :text "Automatic"))

(defun test-packages ()
  "Return a list of test packages, sorted by packaged name."
  (sort (copy-list (parachute:test-packages))
        #'string<
        :key #'package-name))

;;; Tree Callbacks

(defmethod browser-tree-children ((package package))
  (remove-if-not (lambda (test)
                   (and (typep test 'parachute:test)
                        (null (parachute:parent test))))
                 (parachute:package-tests package)))

(defmethod browser-tree-children ((test parachute:test))
  (parachute:children test))

(defmethod browser-tree-print ((item package))
  (string-downcase (package-name item)))

(defmethod browser-tree-print ((item parachute:test))
  (string-downcase (princ-to-string (parachute:name item))))

(defmethod browser-tree-image ((item package))
  :pb-package)

(defmethod browser-tree-image ((item parachute:test))
  :pb-test)

;;; Toolbar Callbacks

(defmethod toolbar-item-enabled-p ((interface test-browser) (name (eql :pb-run-tests)))
  (choice-selected-items (browser-tree interface)))

(defmethod toolbar-item-enabled-p ((interface test-browser) (name (eql :pb-debug)))
  (choice-selected-items (browser-tree interface)))

(defmethod toolbar-item-enabled-p ((interface test-browser) (name (eql :pb-find-source)))
  (choice-selected-items (browser-tree interface)))

(defmethod browser-toolbar-callback ((interface test-browser) (name (eql :pb-refresh)))
  (setf (tree-view-roots (browser-tree interface)) (parachute:test-packages)))

(defmethod browser-toolbar-callback ((interface test-browser) (name (eql :pb-results)))
  (if-let (viewer (locate-interface 'result-viewer))
      (raise-interface viewer)
    (display-message "No test results are currently available for display.")))

(defmethod browser-toolbar-callback ((interface test-browser) (name (eql :pb-run-tests)))
  "run selected tests on a background process, returning results to the current process"
  (when-let (selected (choice-selected-items (browser-tree interface)))
    (execute-tests-in-background selected)))

(defmethod browser-toolbar-callback ((interface test-browser) (name (eql :pb-debug)))
  "run selected tests on a background process using debugging."
  (when-let (selected (choice-selected-items (browser-tree interface)))
    (execute-tests-in-background selected :debug t)))

(defmethod browser-toolbar-callback ((interface test-browser) (name (eql :pb-find-source)))
  "attempt to find source definition for selected item"
  (when-let (selected (first (choice-selected-items (browser-tree interface))))
    (find-source-for-item selected)))

(defmethod browser-toolbar-callback (interface (name (eql :pb-automatic)))
  "Toggle the automatic behaviour flags and display a message indicating state."
  (toggle-automatic-behaviour)
  (display-non-focus-message (format nil
                                     "Refresh: ~A Execute: ~A "
                                     *automatic-refresh*
                                     *automatic-execute*)
                             :owner interface))
