(in-package #:asdf-user)

(defsystem 3bmd-ext-math
  :description "An extension for 3bmd for handling math markup"
  :depends-on (3bmd esrap)
  :serial t
  :license "MIT"
  :author "Lukasz Janyst <lukasz@jany.st>"
  :components ((:file "math")))
