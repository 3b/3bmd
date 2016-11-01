(in-package #:asdf-user)

(defsystem 3bmd-youtube-tests
  :depends-on (#:3bmd-youtube
               #:fiasco)
  :serial t
  :components ((:file "youtube-tests")))
