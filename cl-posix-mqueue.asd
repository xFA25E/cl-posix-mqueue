(asdf:defsystem "cl-posix-mqueue"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cffi" "alexandria")
  :components ((:module "src"
                :components
                ((:file "spec")
                 (:file "utils")
                 (:file "attributes")
                 (:file "lib"))))
  :description ""
  :around-compile (lambda (next)
                    (proclaim '(optimize
                                (compilation-speed 0)
                                (debug 3)
                                (safety 3)
                                (space 0)
                                (speed 0)))
                    (funcall next))
  :in-order-to ((test-op (test-op "cl-posix-mqueue-tests"))))
