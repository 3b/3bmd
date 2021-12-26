(defsystem 3bmd
  :description "markdown processor in CL using esrap parser."
  :depends-on (esrap split-sequence alexandria)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components ((:file "package")
               (:file "parser")
               (:file "extensions")
               (:file "smart-quotes")
               (:file "printer")
               (:file "markdown-printer")
               (:file "plain-printer"))
  :in-order-to ((test-op (test-op 3bmd-tests))))
