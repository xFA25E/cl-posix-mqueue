(asdf:defsystem "cl-posix-mqueue"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cffi")
  :components ((:module "src"
                :components
                ((:file "spec")
                 (:file "utils")
                 (:file "attributes")
                 (:file "lib"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-posix-mqueue-tests"))))
