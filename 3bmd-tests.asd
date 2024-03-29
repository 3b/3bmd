(defsystem "3bmd-tests"
  :depends-on ("3bmd"
               "3bmd-ext-code-blocks"
               "fiasco")
  :serial t
  :components ((:module "tests"
                :components ((:file "package")
                             (:module "grammar"
                              :components ((:file "spaces-and-newlines")
                                           (:module "blocks"
                                            :components ((:file "block-quote")
                                                         (:file "bullet-list")
                                                         (:file "heading")
                                                         (:file "horizontal-rule")
                                                         (:file "html-block")
                                                         (:file "ordered-list")
                                                         (:file "paragraph")
                                                         (:file "plain")
                                                         (:file "reference")
                                                         (:file "verbatim")))
                                           (:module "inlines"
                                            :components ((:file "string")
                                                         (:file "strong")
                                                         (:file "emph")
                                                         (:file "image")
                                                         (:file "link")
                                                         (:file "code")
                                                         (:file "raw-html")
                                                         (:file "entity")
                                                         (:file "escaped-char")
                                                         (:file "ul-or-star")))))
                             (:module "printing"
                              :components ((:file "inline")
                                           (:file "block")))
                             (:module "extensions"
                              :components ((:file "smart-quotes"))))))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (or
                     (symbol-call "FIASCO" "RUN-PACKAGE-TESTS"
                                  :package '#:3bmd-tests)
                     (error "tests failed"))))
