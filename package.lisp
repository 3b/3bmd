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
           ))

(defpackage #:3bmd
  (:use :cl :3bmd-grammar)
  (:export #:parse-string
           #:parse-string-and-print-to-stream
           #:parse-and-print-to-stream
           #:print-doc-to-stream
           #:*smart-quotes*
           ))


(defpackage #:3bmd-ext
  (:use :cl :3bmd-grammar)
  (:import-from :3bmd
                3bmd::print-label-to-string
                3bmd::print-tagged-element
                3bmd::print-element
                3bmd::expand-tabs)
  (:export #:define-extension-inline
           #:print-label-to-string
           #:print-tagged-element
           #:print-element
           #:expand-tabs
           ))

