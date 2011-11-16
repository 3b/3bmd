(defpackage #:3bmd-grammar
  (:use :cl :esrap)
  (:export #:parse-doc
           ;; some rules which might be interesting to parse directly,
           ;; or as reference points for specifying where to add
           ;; extension rules
           ;; todo: add more...
           #:block
           #:string
           #:emph
           ;; normal API stuff
           #:*smart-quotes*
           #:define-extension-inline
           #:define-extension-block
           ))

(defpackage #:3bmd-ext
  (:use :cl :3bmd-grammar)
  (:export #:define-extension-inline
           #:define-extension-block
           #:print-label-to-string
           #:print-tagged-element
           #:print-element
           #:expand-tabs
           ))

(defpackage #:3bmd
  (:use :cl :3bmd-grammar #:3bmd-ext)
  (:export #:parse-string
           #:parse-string-and-print-to-stream
           #:parse-and-print-to-stream
           #:print-doc-to-stream
           #:*smart-quotes*
           #:print-tagged-element
           ))
