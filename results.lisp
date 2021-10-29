(in-package #:parachute-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Result Inspector

(define-interface result-inspector ()
  ((result :initarg :result
           :initform (error "result expected")
           :reader result-inspector-result))
  (:panes
   (report-pane multi-line-text-input-pane
                :visible-min-width '(character 60)
                :font (gp:make-font-description :stock :system-fixed-font))
   (close-button push-button
                 :text "Close"
                 :callback 'quit-interface
                 :callback-type :interface))
  (:layouts
   (main-layout column-layout
                '(report-pane close-button)
                :adjust :right
                :gap 8
                :internal-border 5))
  (:default-initargs
   :title "Test Details"
   :best-width 750
   :best-height 175
   :window-styles '(:never-iconic)))

(defmethod reinitialize-interface ((interface result-inspector)
                                   &rest initargs &key result &allow-other-keys)
  (declare (ignore initargs))
  (when result
    (setf (slot-value interface 'result) result)
    (execute-with-interface interface 'redisplay-interface interface)))

(defmethod redisplay-interface ((interface result-inspector))
  (with-slots (report-pane result) interface
    (setf (text-input-pane-text report-pane)
          (parachute:format-result result :extensive))))

(defmethod interface-display :after ((interface result-inspector))
  (redisplay-interface interface))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Result Viewer

(defparameter +result-viewer-filter-items+ '(:passed :failed :skipped)
  "filter check box items (must match parachute test status keywords")

(define-interface result-viewer (parachute-window)
  ((report :initarg :report
           :initform (error "report required")
           :accessor result-viewer-report)
   (nodes :initform nil
          :accessor result-viewer-nodes)
   (filter :initform +result-viewer-filter-items+
           :reader result-viewer-filter))
  (:panes
   (filter-buttons check-button-panel
                   :reader result-viewer-filter-panel
                   :items +result-viewer-filter-items+
                   :selected-items (result-viewer-filter interface)
                   :callback-type :interface
                   :selection-callback 'result-viewer-filter-change-callback
                   :retract-callback 'result-viewer-filter-change-callback
                   :print-function 'string-capitalize)
   (result-tree extended-selection-tree-view
                :reader result-viewer-tree
                :children-function (curry 'result-viewer-children interface)
                :print-function 'result-viewer-print
                :image-function 'result-viewer-image
                :expandp-function (constantly t)
                :selection-callback :redisplay-interface
                :retract-callback :redisplay-interface
                :callback-type :interface-data
                :action-callback 'result-viewer-inspect-result
                :alternating-background t)
   (status-line title-pane
                :reader result-viewer-status-line)
   (icons-copyright title-pane
                    :text "Icons by https://icons8.com/"
                    :font (gp:make-font-description :stock :system-font :size 10)))
  (:layouts
   (main-layout column-layout
                '(filter-buttons result-tree bottom-layout)
                :gap 5
                :ratios '(nil 1 nil)
                :internal-border 5)
   (bottom-layout row-layout
                  '(status-line nil icons-copyright)
                  :gap 5
                  :adjust :center))
  (:default-initargs
   :title "Test Results"
   :toolbar-items (result-viewer-toolbar)))

(define-toolbar (result-viewer-toolbar 'result-viewer-toolbar-callback)
  (:name :pb-run-tests :text "Run Again")
  (:name :pb-browser :text "Browser" :callback 'find-interface
   :callback-type :data :data 'test-browser))

(defmethod reinitialize-interface ((interface result-viewer)
                                   &rest initargs &key report &allow-other-keys)
  (setf (result-viewer-report interface) report)
  (execute-with-interface interface 'result-viewer-redisplay-tree interface))

(defmethod interface-display :after ((interface result-viewer))
  (result-viewer-redisplay-tree interface))

;;; Toolbar Callbacks

(defmethod result-viewer-toolbar-callback ((interface result-viewer) (name (eql :pb-run-tests)))
  (with-slots (result-tree) interface
    (when-let (selected (delete nil (mapcar (lambda (item)
                                              (when (typep item 'result-node)
                                                (result-node-test item)))
                                            (or (choice-selected-items result-tree)
                                                (tree-view-roots result-tree)))))
      (execute-tests-in-background selected))))

;;; Tree Display & Filtering

(defun result-viewer-redisplay-tree (interface)
  (with-slots (report nodes result-tree status-line) interface
    (when report
      (multiple-value-bind (roots node-table)
          (build-result-tree (parachute:results report))
        (setf nodes node-table
              (tree-view-roots result-tree) roots
              (title-pane-text status-line) (brief-summary report))))))

(defun result-viewer-update-tree-with-filter (interface)
  ;; reset the root items to force redisplay without rebuilding entire tree.
  (with-slots (result-tree) interface
    (setf (tree-view-roots result-tree) (tree-view-roots result-tree))))

(defun apply-result-filter (interface results)
  ;; include :UNKNOWN in filter so result items without a status are always shown
  (let ((filter (cons :unknown (result-viewer-filter interface))))
    (delete-if-not (lambda (result)
                     (etypecase result
                       (result-node (member (result-node-status result) filter))
                       (parachute:result (member (parachute:status result) filter))))
                   (coerce results 'list))))

(defun result-viewer-filter-change-callback (interface)
  (with-slots (filter filter-buttons) interface
    (setf filter (choice-selected-items filter-buttons)))
  (result-viewer-update-tree-with-filter interface))

;;; Tree Callbacks

(defmethod result-viewer-children ((interface result-viewer) (node result-node))
  (apply-result-filter interface
                       (or (result-node-children node)
                           (parachute:results (result-node-result node)))))

(defmethod result-viewer-children ((interface result-viewer) (result parachute:value-result))
  nil)

(defmethod result-viewer-children ((interface result-viewer) (result parachute:parent-result))
  (apply-result-filter interface (parachute:results result)))

(defmethod result-viewer-print ((node result-node))
  (princ-to-string (parachute:name (result-node-test node))))

(defmethod result-viewer-print ((node parachute:result))
  (parachute:format-result node :oneline))

(defun result-viewer-image (node)
  (case (etypecase node
          (result-node (result-node-status node))
          (parachute:test (parachute:status node))
          (parachute:result (parachute:status node)))
    (:passed :pb-passed)
    (:failed :pb-failed)
    (:skipped :pb-skipped)
    (t :pb-unknown)))

;;; Result Inspection

(defmethod result-viewer-inspect-result (interface item)
  (declare (ignore interface item))
  (display-message "Result information for this item is not available."))

(defmethod result-viewer-inspect-result ((interface result-viewer) (item result-node))
  (result-viewer-inspect-result interface (result-node-result item)))

(defmethod result-viewer-inspect-result ((interface result-viewer) (item parachute:result))
  (find-interface 'result-inspector :result item))
