(in-package #:3bmd)

;;; "plain text" printer
;;; prints the text contents of the document with no formatting

(defparameter *plain-text-pprint* (copy-pprint-dispatch nil))

(set-pprint-dispatch 'cons
                     (lambda (s o)
                       (loop for i in o
                             do (write i :stream s)))
                     0
                     *plain-text-pprint*)

(set-pprint-dispatch 'string
                     (lambda (s o)
                       (write o :stream s :pretty nil :escape nil))
                     0
                     *plain-text-pprint*)

(defmacro pprinter (tag (stream-var object-var) &body body)
  `(let ((*print-pprint-dispatch* *plain-text-pprint*))
     (set-pprint-dispatch '(cons (eql ,tag))
                          (lambda (,stream-var ,object-var) ,@body)
                          1)))

(pprinter :heading (s o)
  (format s "~a " (getf (cdr o) :contents)))

(pprinter :paragraph (s o)
  (write (cdr o) :stream s))

(pprinter :verbatim (s o)
  (format s "~{[~a]~}" (cdr o)))

(pprinter :code (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :block-quote (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :plain (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :strong (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :link (s o)
  (format s "~{~a ~}" (cadr o)))

(pprinter :mailto (s o)
  (format s "~{~a ~}" (cadr o)))

(pprinter :explicit-link (s o)
  (format s "~{~a ~}" (getf (cdr o) :label)))

(pprinter :reference-link (s o)
  (format s "~{~a ~}" (getf (cdr o) :label)))

(pprinter :image (s o)
  (format s "~{~a ~}" (getf (cdr o) :label)))

(pprinter :counted-list (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :bullet-list (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :list-item (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :line-break (s o)
  (format s "~%"))

(pprinter :horizontal-rule (s o)
  (format s "----- "))


(pprinter :html (s o)
  ;; should this print anything?
  )

(pprinter :raw-html (s o)
  ;; should this print anything?
  )


(pprinter :single-quoted (s o)
  (format s "'~{~a ~}'" (cdr o)))

(pprinter :double-quoted (s o)
  (format s "\"~{~a ~}\"" (cdr o)))

(pprinter :em-dash (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :en-dash (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :left-right-single-arrow (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :left-single-arrow (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :right-single-arrow (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :left-right-double-arrow (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :left-double-arrow (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :right-double-arrow (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :ellipsis (s o)
  (format s "~{~a ~}" (cdr o)))

(pprinter :reference (s o)
  ;?(format s "~{~a ~}" (cdr o))
  )

(pprinter :apostrophe (s o)
  (format s "'"))







(defmethod print-doc-to-stream-using-format (doc stream (format (eql :plain)))
  (let ((*references* (extract-refs doc)))
    (with-standard-io-syntax
      (let* ((*print-pprint-dispatch* *plain-text-pprint*)
             (*print-pretty* t)
             (*print-readably* nil)
             (old-debug *debugger-hook*)
             (*debugger-hook* (lambda (&rest r)
                                (with-standard-io-syntax
                                  (apply old-debug r)))))
        (write doc :stream stream)))
    (format stream "~&")))

