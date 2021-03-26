(asdf:defsystem "cl-posix-mqueue-tests"
  :depends-on ("cl-posix-mqueue" "rove" "cl-ppcre")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-posix-mqueue"
  :perform (asdf:test-op (op c) (symbol-call :rove :run c)))
