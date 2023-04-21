(defsystem 3bmd-ext-tables
  :description "Extension to 3bmd implementing PHP Markdown Extra style tables"
  :depends-on (3bmd)
  :serial t
  :components ((:file "tables"))
  :in-order-to ((test-op (test-op 3bmd-ext-tables/tests))))

(defsystem 3bmd-ext-tables/tests
  :depends-on (#:3bmd-ext-tables #:3bmd-tests #:fiasco)
  :serial t
  :components ((:module "tests"
                :components ((:module "extensions"
                              :components ((:file "tables"))))))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (or
                     (symbol-call "FIASCO" "RUN-PACKAGE-TESTS"
                                  :package '#:3bmd-table-tests)
                     (error "tests failed"))))
