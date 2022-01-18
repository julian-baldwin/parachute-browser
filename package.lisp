(in-package #:cl-user)

(defpackage #:parachute-browser
  (:add-use-defaults t)
  (:use #:capi)
  (:import-from #:alexandria #:curry #:rcurry #:ensure-gethash)
  (:export #:test-browser #:result-viewer #:execute-tests-in-background #:browse-tests
           #:load-examples #:define-test
           #:*automatic-refresh* #:*automatic-execute* #:toggle-automatic-behaviour))
