(in-package #:asdf-user)

(defsystem 3bmd-youtube-tests
  :depends-on (#:3bmd-youtube
               #:fiasco)
  :serial t
  :components ((:file "youtube-tests"))
  :perform (test-op (op c)
                    (declare (ignore op c))
                    (or
                     (symbol-call "FIASCO" "RUN-PACKAGE-TESTS"
                                  :package '#:3bmd-youtube-tests)
                     (error "tests failed"))))
