(defpackage #:3bmd-grammar
  (:use :cl :esrap)
  (:export #:parse-doc
           ;; possibly should export some symbols for direct parsing too?
           ;; #:doc #:block
           #:*smart-quotes*
           #:*wiki-links*
           ))

(defpackage #:3bmd
  (:use :cl :3bmd-grammar)
  (:export #:parse-string
           #:parse-string-and-print-to-stream
           #:parse-and-print-to-stream
           #:print-doc-to-stream
           #:process-wiki-link
           #:*smart-quotes*
           #:*wiki-links*
           #:*wiki-processor*
           ))


