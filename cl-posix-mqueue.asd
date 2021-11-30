(defsystem "cl-posix-mqueue"
  :author "Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>"
  :maintainer "Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>"
  :license "GPL3"
  :version "0.1.2"
  :depends-on ("cffi" "alexandria" "babel" "local-time")
  :components ((:module "src"
                :components ((:file "package")
                             (:file "condition")
                             (:file "queue")
                             (:file "types")
                             (:file "translation" :depends-on ("queue" "types"))
                             (:file "spec" :depends-on ("queue" "translation" "types"))
                             (:file "posix-mqueue" :depends-on ("condition" "queue" "spec" "types")))))
  :description "POSIX message queue bindings for Common Lisp"
  :in-order-to ((test-op (test-op "cl-posix-mqueue/tests")))
  :long-description #.(let ((file (probe-file* (subpathname *load-pathname* "README.md")))) (when file (read-file-string file))))

(defsystem "cl-posix-mqueue/tests"
  :depends-on ("cl-posix-mqueue" "fiveam" "cl-ppcre")
  :components ((:module "tests" :components ((:file "posix-mqueue"))))
  :description "Test system for cl-posix-mqueue"
  :perform (test-op (op c) (symbol-call '#:fiveam '#:run! (find-symbol* '#:posix-mqueue '#:posix-mqueue.tests))))
