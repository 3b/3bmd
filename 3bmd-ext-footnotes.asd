(defsystem 3bmd-ext-footnotes
  :description "extension to 3bmd implementing github style ``` delimited code blocks, with support for syntax highlighting using colorize, pygments, or chroma"
  :depends-on (3bmd alexandria)
  :serial t
  :components ((:file "footnotes"))
  :in-order-to ((test-op (test-op 3bmd-ext-footnotes/tests))))


(defsystem 3bmd-ext-footnotes/tests
  :depends-on (#:3bmd-ext-footnotes #:3bmd-tests #:fiasco)
  :serial t
  :components ((:module "tests"
                :components ((:module "extensions"
                              :components ((:file "footnotes"))))))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (or
                     (symbol-call "FIASCO" "RUN-PACKAGE-TESTS"
                                  :package '#:3bmd-footnotes-tests)
                     (error "tests failed"))))
