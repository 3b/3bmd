(defsystem 3bmd-ext-definition-lists
  :description "extension to 3bmd implementing 'PHP Markdown Extra' style definition lists"
  :depends-on (3bmd colorize alexandria)
  :serial t
  :components ((:file "definition-lists"))
  :in-order-to ((test-op (test-op 3bmd-ext-definition-lists/tests))))

(defsystem 3bmd-ext-definition-lists/tests
  :depends-on (#:3bmd-ext-definition-lists #:3bmd-tests #:fiasco)
  :serial t
  :components ((:module "tests"
                :components ((:module "extensions"
                              :components ((:file "definition-lists"))))))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (or
                     (symbol-call "FIASCO" "RUN-PACKAGE-TESTS"
                                  :package '#:3bmd-definition-list-tests)
                     (error "tests failed"))))
