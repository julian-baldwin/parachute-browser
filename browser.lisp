(in-package #:parachute-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Browser

(define-interface test-browser (parachute-window) ()
  (:panes
   (test-tree extended-selection-tree-view
              :reader browser-tree
              :roots (parachute:test-packages)
              :children-function 'browser-tree-children
              :print-function 'browser-tree-print
              :image-function 'browser-tree-image
              :alternating-background t
              :callback-type :interface
              :action-callback (rcurry 'browser-toolbar-callback :pb-run-tests)
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
   :toolbar-items (browser-toolbar)))

(define-toolbar (browser-toolbar 'browser-toolbar-callback)
  (:name :pb-run-tests :text "Run")
  (:name :pb-results :text "Results")
  (:name :pb-refresh :text "Refresh"))

;;; Tree Callbacks

(defmethod browser-tree-children ((package package))
  (remove-if-not (lambda (test)
                   (and (typep test 'parachute:test)
                        (null (parachute:parent test))))
                 (parachute:package-tests package)))

(defmethod browser-tree-children ((test parachute:test))
  (parachute:children test))

(defmethod browser-tree-print ((item package))
  (package-name item))

(defmethod browser-tree-print ((item parachute:test))
  (princ-to-string (parachute:name item)))

(defmethod browser-tree-image ((item package))
  :pb-package)

(defmethod browser-tree-image ((item parachute:test))
  :pb-test)

;;; Toolbar Callbacks

(defmethod toolbar-item-enabled-p ((interface test-browser) (name (eql :pb-run-tests)))
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
