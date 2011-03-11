(defsystem 3bmd
  :depends-on (esrap split-sequence alexandria)
  :serial t
  :components ((:file "package")
               (:file "parser")
               (:file "printer")))
