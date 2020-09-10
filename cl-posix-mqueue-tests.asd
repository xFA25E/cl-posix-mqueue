(asdf:defsystem "cl-posix-mqueue-tests"
  :author ""
  :license ""
  :depends-on ("cl-posix-mqueue" "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-posix-mqueue"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))
