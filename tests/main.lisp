(defpackage posix-mqueue/tests/main
  (:use :cl
        :cl-posix-mqueue
        :rove))
(in-package :posix-mqueue/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-posix-mqueue)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
