(in-package #:cl-user)

(load (current-pathname "package.lisp"))

(defsystem "parachute-browser"
  (:package #:parachute-browser
   :optimize ((speed 3) (safety 1)))
  :rules ((:in-order-to :compile :all (:requires (:load :previous))))
  :members ("utilities" "browser" "results"))
