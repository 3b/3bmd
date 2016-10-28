(in-package #:asdf-user)

(defsystem 3bmd-youtube
  :description "An extension for 3bmd for embedding YouTube videos"
  :depends-on (3bmd esrap)
  :serial t
  :components ((:file "youtube"))
  :in-order-to ((test-op (load-op 3bmd-youtube-tests)))
  :perform (test-op :after (op c)
                    (declare (ignore op c))
                    (uiop:symbol-call :fiasco 'run-package-tests :package :3bmd-youtube-tests)))
