(defsystem 3bmd-tests
  :depends-on (#:3bmd
               #:fiasco)
  :serial t
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "grammar"))))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (symbol-call "FIASCO" "RUN-PACKAGE-TESTS"
                                 :package '#:3bmd-tests)))
