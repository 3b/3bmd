(in-package #:asdf-user)

(defsystem 3bmd-youtube
  :description "An extension for 3bmd for embedding YouTube videos"
  :depends-on (3bmd esrap)
  :serial t
  :components ((:file "youtube"))
  :in-order-to ((test-op (test-op 3bmd-youtube-tests))))
