(in-package #:cl-user)

(defpackage #:parachute-browser/examples
  (:add-use-defaults t)
  (:shadowing-import-from #:parachute
                          #:true
                          #:false
                          #:is
                          #:fail
                          #:finish
                          #:skip
                          #:define-test))

(in-package #:parachute-browser/examples)

(define-test example-tests)
(define-test an-example-suite :parent example-tests)
(define-test another-example-suite :parent example-tests)

(define-test test-something :parent an-example-suite
  (is equal 1 1)
  (true t)
  (false t))

(define-test test-something-else :parent an-example-suite
  (fail (error "this test will pass")))

(define-test test-many-things :parent another-example-suite
  (loop for i from 1 to 10
        doing (if (= 0 (mod i 3))
                  (skip "skipped this bit" (false t))
                  (true (evenp i) "checking if ~A is even" i))))
