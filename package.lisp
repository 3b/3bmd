(defpackage #:3bmd-grammar
  (:use :cl :esrap)
  (:export #:parse-doc
           #:inline-extensions
           ;; possibly should export some symbols for direct parsing too?
           ;; #:doc #:block
           ))

(defpackage #:3bmd
  (:use :cl)
  (:export #:parse-string
           #:parse-string-and-print-to-stream
           #:parse-and-print-to-stream
           #:print-doc-to-stream
           #:print-tagged-element
           ))


