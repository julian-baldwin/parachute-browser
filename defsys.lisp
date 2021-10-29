(in-package #:cl-user)

(defpackage #:parachute-browser
  (:add-use-defaults t)
  (:use #:capi)
  (:import-from #:alexandria #:curry #:rcurry #:ensure-gethash)
  (:export #:test-browser #:result-viewer #:execute-tests-in-background #:browse-tests))

(defsystem "parachute-browser"
  (:package #:parachute-browser
   :optimize ((speed 3) (safety 1)))
  :rules ((:in-order-to :compile :all (:requires (:load :previous))))
  :members ("utilities" "browser" "results"))
