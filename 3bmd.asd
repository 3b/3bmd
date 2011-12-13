(defsystem 3bmd
  :description "markdown processor in CL using esrap parser."
  :depends-on (esrap split-sequence alexandria)
  :serial t
  :components ((:file "package")
               (:file "parser")
               (:file "extensions")
               (:file "smart-quotes")
               (:file "printer")))
