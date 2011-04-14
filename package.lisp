(defpackage #:3bmd-grammar
  (:use :cl :esrap)
  (:export #:parse-doc
           ;; possibly should export some symbols for direct parsing too?
           ;; #:doc #:block
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
                3bmd::print-element)
  (:export #:define-extension-inline
           #:print-label-to-string
           #:print-tagged-element
           #:print-element
           ))

