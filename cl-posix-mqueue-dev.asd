(asdf:defsystem "cl-posix-mqueue-dev"
  :depends-on ("cffi" "alexandria" "babel" "local-time")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "queue")
                 (:file "types")
                 (:file "translation")
                 (:file "spec")
                 (:file "condition")
                 (:file "lib"))))
  :description "Development system for cl-posix-mqueue"
  :around-compile
  (lambda (next)
    (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))
    (funcall next))
  :in-order-to ((test-op (test-op "cl-posix-mqueue-tests"))))
