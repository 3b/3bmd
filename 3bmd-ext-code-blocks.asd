(defsystem 3bmd-ext-code-blocks
  :description "extension to 3bmd implementing github style ``` delimited code blocks, with support for syntax highlighting using colorize, pygments, or chroma"
  :depends-on (3bmd colorize alexandria split-sequence #-asdf3 :uiop)
  :serial t
  :components ((:file "code-blocks")
               (:static-file "pygmentize.py"))
  :in-order-to ((test-op (test-op 3bmd-ext-code-blocks/tests))))


(defsystem 3bmd-ext-code-blocks/tests
  :depends-on (#:3bmd-ext-code-blocks #:3bmd-tests #:fiasco)
  :serial t
  :components ((:module "tests"
                :components ((:module "extensions"
                              :components ((:file "code-blocks"))))))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (or
                     (symbol-call "FIASCO" "RUN-PACKAGE-TESTS"
                                  :package '#:3bmd-code-block-tests)
                     (error "tests failed"))))
