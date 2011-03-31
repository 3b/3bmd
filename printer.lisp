(in-package #:3bmd)

(defparameter *references* nil)
(defparameter *always-title* nil)
(defparameter *padding* 2)
(defmacro padded ((n s &optional (next 0)) &body body)
  (alexandria:once-only (n s next)
    `(progn
       (loop for ,n from ,n above *padding* do (format ,s "~%"))
       (setf *padding* ,n)
       (prog1
           (progn ,@body)
         (setf *padding* ,next)))))

(defun print-label-to-string (label)
  (with-output-to-string (s)
    (loop with spaces = (coerce '(#\space #\tab #\newline #\return #\linefeed)
                                'string)
       for was-space = nil then space
       for c across (with-output-to-string (s)
                      (loop for i in (alexandria:ensure-list label)
                         do (print-element i s)))
       for space = (find c spaces)
       when space
       do (unless was-space (write-char #\space s))
       else do (write-char c s))))

(defun lookup-reference (ref)
  #++(format t "lookup ref ~s -> ~s~%" (print-label-to-string ref)
          (gethash (print-label-to-string ref) *references*))
  (if *references*
      (gethash (print-label-to-string ref) *references*)))


(defun print-escaped (string stream)
  (loop for c across string
     when (eql c #\&) do (write-string "&amp;" stream)
     else when (eql c #\<) do (write-string "&lt;" stream)
     else when (eql c #\>) do (write-string "&gt;" stream)
     else when (eql c #\") do (write-string "&quot;" stream)
     ;; todo: 'obfuscate' email addresses (when *obfuscate* ...)
     else do (write-char c stream)))

(defun escape-string (string)
  (when string
    (with-output-to-string (s)
      (print-escaped string s))))

;; todo: minimize extra newlines...
(defmethod print-tagged-element ((tag (eql :heading)) stream rest)
  (padded (2 stream)
    (format stream "<h~d>" (getf rest :level))
    (mapcar (lambda (a) (print-element a stream)) (getf rest :contents))
    (format stream "</h~d>" (getf rest :level))))

(defmethod print-tagged-element ((tag (eql :paragraph)) stream rest)
  (padded (2 stream)
    (format stream "<p>")
    (mapcar (lambda (a) (print-element a stream)) rest)
    (format stream "</p>")))

(defmethod print-tagged-element ((tag (eql :block-quote)) stream rest)
  (padded (2 stream)
    (format stream "<blockquote>~%")
    (mapcar (lambda (a) (print-element a stream)) rest)
    (format stream "~&</blockquote>")))

(defmethod print-tagged-element ((tag (eql :plain)) stream rest)
  (padded (1 stream)
    (mapcar (lambda (a) (print-element a stream)) rest)))


(defmethod print-tagged-element ((tag (eql :emph)) stream rest)
  (format stream "<em>")
  (mapcar (lambda (a) (print-element a stream)) rest)
  (format stream "</em>"))

(defmethod print-tagged-element ((tag (eql :strong)) stream rest)
  (format stream "<strong>")
  (mapcar (lambda (a) (print-element a stream)) rest)
  (format stream "</strong>"))

(defmethod print-tagged-element ((tag (eql :link)) stream rest)
  (format stream "<a href=\"~a\">~a</a>" (car rest) (car rest)))

(defun encode-email (text)
  (with-output-to-string (s)
    (loop for i across text
       for r = (random 1.0)
       do (cond
            ((< r 0.1) (write-char i s))
            ;; fixme: make this portable to non-unicode/ascii lisps?
            ((< r 0.6) (format s "&#x~x;" (char-code i)))
            (t (format s "&#~d;" (char-code i)))))))

(defmethod print-tagged-element ((tag (eql :mailto)) stream rest)
  (format stream "<a href=\"~a\">~a</a>" (encode-email (car rest))
          (encode-email (car rest))))

(defmethod print-tagged-element ((tag (eql :explicit-link)) stream rest)
  (format stream "<a href=\"~a\" ~@[title=\"~a\"~]>"
          (getf rest :source)
          (escape-string
           (or (getf rest :title) (if *always-title* "" nil))))
  (mapcar (lambda (a) (print-element a stream)) (getf rest :label))
  (format stream "</a>"))

(defmethod print-tagged-element ((tag (eql :reference-link)) stream rest)
  (let* ((label (getf rest :label))
         (def (or (getf rest :definition) label))
         (ref (lookup-reference def)))
    (cond
      (ref
       (format stream "<a href=\"~a\" ~@[title=\"~a\"~]>" (first ref)
               (escape-string (or (second ref) (if *always-title* "" nil))))
       (mapcar (lambda (a) (print-element a stream)) label)
       (format stream "</a>"))
      (t
       (format stream "[")
       (mapcar (lambda (a) (print-element a stream)) label)
       (format stream "]~@[~a~]" (getf rest :tail))))))

(defmethod print-tagged-element ((tag (eql :image)) stream rest)
  (setf rest (cdr (first rest)))
  (format stream "<img src=\"~a\" ~@[alt=\"~a\"~] ~@[title=\"~a\"~]/>"
          (getf rest :source)
          (with-output-to-string (s)
            (mapcar (lambda (a) (print-element a s)) (getf rest :label)))
          (escape-string
           (or (getf rest :title) (if *always-title* "" nil)))))


(defmethod print-tagged-element ((tag (eql :counted-list)) stream rest)
  (padded (2 stream)
    (format stream "<ol>"))
  (mapcar (lambda (a) (print-element a stream)) rest)
  (padded (1 stream)
    (format stream "~&</ol>")))

(defmethod print-tagged-element ((tag (eql :bullet-list)) stream rest)
  (padded (2 stream)
    (format stream "<ul>"))
  (mapcar (lambda (a) (print-element a stream)) rest)
  (padded (1 stream)
    (format stream "</ul>")))

(defmethod print-tagged-element ((tag (eql :list-item)) stream rest)
  (padded (1 stream 2)
    (format stream "<li>"))
  (mapcar (lambda (a) (print-element a stream)) rest)
  (format stream "</li>")
  (setf *padding* 0))

(defmethod print-tagged-element ((tag (eql :line-break)) stream rest)
  (format stream "<br/>~%"))

(defmethod print-tagged-element ((tag (eql :horizontal-rule)) stream rest)
  (padded (2 stream)
    (format stream "<hr/>")))


(defmethod print-tagged-element ((tag (eql :html)) stream rest)
  (padded (2 stream)
    (format stream "~{~a~}" rest)))

(defmethod print-tagged-element ((tag (eql :raw-html)) stream rest)
  (format stream "~{~a~}" rest))

(defmethod print-tagged-element ((tag (eql :entity)) stream rest)
  (format stream "~{~a~}" rest))

(defmethod print-tagged-element ((tag (eql :verbatim)) stream rest)
  (padded (2 stream)
    (format stream "<pre><code>")
    (mapcar (lambda (a) (print-element a stream)) rest)
    (format stream "</code></pre>")))

;;; track whether we are in a code block, so we can avoid smart-quote
;;; junk if it was enabled in the parser
;;;
;;; possibly should just check the *smart-quotes* var from 3bmd-grammar
;;; and bind that to nil inside code blocks?
;;; (or make parser smart enough to not match inside code blocks)
(defparameter *in-code* nil)

(defmethod print-tagged-element ((tag (eql :code)) stream rest)
  (format stream "<code>")
  (let ((*in-code* t))
    (mapcar (lambda (a) (print-element a stream)) rest))
  (format stream "</code>"))

(defmethod print-tagged-element ((tag (eql :single-quoted)) stream rest)
  (if *in-code*
      (format stream "'")
      (format stream "&lsquo;"))
  (mapcar (lambda (a) (print-element a stream)) rest)
  (if *in-code*
      (format stream "'")
      (format stream "&rsquo;")))

(defmethod print-tagged-element ((tag (eql :double-quoted)) stream rest)
  (if *in-code*
      (format stream "\"")
      (format stream "&ldquo;"))
  (mapcar (lambda (a) (print-element a stream)) rest)
  (if *in-code*
      (format stream "\"")
      (format stream "&rdquo;")))

(defmacro define-smart-quote-entity (name replacement)
  `(defmethod print-tagged-element ((tag (eql ,name)) stream rest)
     (if *in-code*
         (format stream "~{~a~}" rest)
         (format stream ,replacement))))

(define-smart-quote-entity :em-dash "&mdash;")
(define-smart-quote-entity :en-dash "&ndash;")
(define-smart-quote-entity :left-right-single-arrow "&harr;")
(define-smart-quote-entity :left-single-arrow "&larr;")
(define-smart-quote-entity :right-single-arrow "&rarr;")
(define-smart-quote-entity :left-right-double-arrow "&hArr;")
(define-smart-quote-entity :left-double-arrow "&lArr;")
(define-smart-quote-entity :right-double-arrow "&rArr;")

(defmethod print-tagged-element ((tag (eql :ellipsis)) stream rest)
  (if *in-code*
      (format stream "~{~a~}" rest)
      (format stream "&hellip;")))

(defmethod print-tagged-element ((tag (eql :reference)) stream rest)
  )

(defmethod print-element ((elem (eql :apostrophe)) stream)
  (if *in-code*
      (format stream "'")
      (format stream "&apos;")))

(defmethod print-element ((elem string) stream)
  #++(format stream "~a" elem)
  (print-escaped elem stream))

(defmethod print-element ((elem cons) stream)
  (if (symbolp (car elem))
      (print-tagged-element (car elem) stream (cdr elem))
      (error "unknown cons? ~s" elem)))



(defun extract-refs (doc)
  (alexandria:alist-hash-table
   (loop for i in doc
      when (and (consp i) (eq (car i) :reference))
      collect (list (print-label-to-string (getf (cdr i) :label))
                    (getf (cdr i) :source)
                    (getf (cdr i) :title)))
   :test #'equalp))

(defun expand-tabs (doc &key add-newlines)
  (with-output-to-string (s)
    (let ((pos 0))
      (flet ((out (c)
               (incf pos)
               (write-char c s)))
       (loop
          for i across doc
          do (case i
               ((#\newline)
                (setf pos 0)
                (write-char i s))
               ((#\tab)
                (loop repeat (- 4 (mod pos 4)) do (out #\space)))
               (t (out i))))))
    (when add-newlines
      (format s "~%~%"))))

(defun print-doc-to-stream (doc stream)
  (let ((*references* (extract-refs doc)))
    (loop for i in doc
       do (print-element i stream))
    (format stream "~&")))

(defun parse-and-print-to-stream (file stream)
  (let* ((input (expand-tabs (alexandria:read-file-into-string file)
                             :add-newlines t))
         (doc (3bmd-grammar::parse-doc input)))
    (print-doc-to-stream doc stream)))

(defun parse-string-and-print-to-stream (string stream)
  (let* ((input (expand-tabs string :add-newlines t)))
    (print-doc-to-stream (3bmd-grammar::parse-doc input) stream)))

