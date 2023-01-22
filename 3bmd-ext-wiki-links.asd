(defsystem 3bmd-ext-wiki-links
  :description "example extension to 3bmd implementing simple wiki-style [[links]]"
  :depends-on (3bmd)
  :serial t
  :components ((:file "wiki-links"))
  :in-order-to ((test-op (test-op 3bmd-ext-wiki-links/tests))))


(defsystem 3bmd-ext-wiki-links/tests
  :depends-on (#:3bmd-ext-wiki-links #:3bmd-tests #:fiasco)
  :serial t
  :components ((:module "tests"
                :components ((:module "extensions"
                              :components ((:file "wiki-links"))))))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (or
                     (symbol-call "FIASCO" "RUN-PACKAGE-TESTS"
                                  :package '#:3bmd-wiki-links-tests)
                     (error "tests failed"))))
