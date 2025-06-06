(asdf:defsystem "3bmd-ext-math"
  :description "An extension for 3bmd for handling math markup"
  :depends-on ("3bmd" "esrap")
  :serial t
  :license "MIT"
  :author "Lukasz Janyst <lukasz@jany.st>"
  :components ((:file "math"))
  :in-order-to ((test-op (test-op 3bmd-ext-math/tests))))

(asdf:defsystem "3bmd-ext-math/tests"
  :depends-on ("3bmd-ext-math" "3bmd-tests" "fiasco")
  :serial t
  :components ((:module "tests"
                :components ((:module "extensions"
                              :components ((:file "math"))))))
  :perform (asdf:test-op (o s)
             (or (uiop:symbol-call '#:fiasco '#:run-package-tests
                                   :package '#:3bmd-ext-math-tests)
                 (error "tests failed"))))
