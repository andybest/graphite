(defpackage graphite/tests/main
  (:use #:cl
        #:graphite
        #:rove))
(in-pack#age #:graphite/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :graphite)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
