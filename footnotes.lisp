(defpackage #:3bmd-footnote
  (:use :cl :esrap :3bmd-ext)
  (:export #:*footnotes*))

(in-package #:3bmd-footnote)

(defvar *footnotes*)

;; fixme: add an extension API for this
(pushnew '*footnotes* 3bmd::*footers*)

(defrule footnote (and "[^" (+ (and (! (or #\[ #\]))
                                    3bmd-grammar::non-space-char))
                       "]")
  (:function second)
  (:text t))

(define-extension-inline *footnotes* footnote-ref
    (and footnote (! #\:))
  (:function first)
  (:lambda (id)
    (list 'footnote-ref id)))

;;; we parse footnote definitions basically like a loose list, but
;;; without a "next list item"
(defrule footnote-block (and (! 3bmd-grammar::blank-line)
                             3bmd-grammar::line
                             (* footnote-block-line))
  (:function rest)
  (:destructure (l block)
    (text l block)))

(defrule footnote-continuation-block (and (* 3bmd-grammar::blank-line)
                                          (+ (and 3bmd-grammar::indent
                                                  footnote-block)))
  (:destructure (b c)
    (if b
        (cons (text b) (mapcar 'second c))
        (cons :split (mapcar 'second c)))))

(defrule footnote-block-line (and (! 3bmd-grammar::blank-line)
                                  (! 3bmd-grammar::horizontal-rule)
                                  3bmd-grammar::optionally-indented-line)
  (:function third))


;; hash table of ID -> (ref-name . backref-names)
(defvar *used-footnotes*)
(defvar *defined-footnotes*)
(defvar *next-ref*)

(define-extension-block *footnotes* footnote-def
    (and (and footnote #\:)
         footnote-block
         (* footnote-continuation-block))
  (:before 3bmd-grammar::reference)
  (:bind *used-footnotes* (make-hash-table :test 'equalp)
         *next-ref* 1)
  (:destructure
   ((id c) block cont)
   (declare (ignore c))
   (list* 'footnote-def
          id
          (loop for a in (split-sequence:split-sequence
                          :split (append (cons block (mapcan 'identity cont))
                                         (list "

"))
                          :remove-empty-subseqs t)
                for p = (3bmd::parse-doc (text a))
                ;; we append a node for the back links to last item if
                ;; it is a paragraph, or after last item otherwise
                if (typep (car (last p)) '(cons (eql :paragraph)))
                  do (push '(footnote-backlinks)
                           (cdr (last (car (last p)))))
                else do (setf p (append p '((footnote-backlinks))))
                append p))))


(defmethod 3bmd::extract-ref ((id (eql 'footnote-def)) cdr)
  (when *footnotes*
    (list* (list 'footnote-def (first cdr))
           cdr)))

(defmethod print-tagged-element ((tag (eql 'footnote-ref)) stream rest)
  (let* ((id (car rest))
         (use (gethash id *used-footnotes*))
         (refno (or (first use)
                    (shiftf *next-ref* (1+ *next-ref*))))
         (backrefs (cddr use))
         (back (if backrefs
                   (format nil "fnref-~a.~a" refno (1+ (length backrefs)))
                   (format nil "fnref-~a" refno)))
         (fn (or (second use)
                 (second
                  (setf (gethash id *used-footnotes*)
                        (list refno (format nil "fn-~a" refno)))))))
    (push back (cddr (gethash id *used-footnotes*)))
    (format stream "<sup><a href=\"#~a\" id=\"~a\" >~a</a></sup>"
            fn back
            refno)))

(defmethod print-tagged-element ((tag (eql 'footnote-def)) stream rest)
  ;; definitions will be printed in the footer, so ignore them here
  )

(defvar *backlinks* nil)
(defmethod print-tagged-element ((tag (eql 'footnote-backlinks)) stream rest)
  (loop for b in *backlinks*
        do (format stream " <a href=\"#~a\" class=\"footnote-back\">↩︎</a>" b)))


(defmethod print-def (s refs (format (eql :html)))
  (let ((def (gethash (list 'footnote-def (first refs)) 3bmd::*references*))
        (*backlinks* (reverse (cdddr refs))))
    (format s "<li id=\"~a\">" (third refs))
    (3bmd::padded (2 s)
      (loop for i in (cdr def)
            do (3bmd::print-element i s)))
    (format s "</li>")))

(defmethod 3bmd::print-footer (stream (f (eql '*footnotes*)) format)
  (when (plusp (hash-table-count *used-footnotes*))
    (format stream "<section \"id=footnotes\" class=\"footnotes\">")
    (3bmd::padded (2 stream)
      (format stream "<hr />")
      (format stream "<ol>")
      (3bmd::padded (2 stream)
        (loop for refs in (sort (alexandria:hash-table-alist *used-footnotes*)
                                '< :key 'second)
              do (print-def stream refs format)))
      (format stream "</ol>"))
    (format stream "</section>")))

#++
(let ((*footnotes* t)
      (3bmd-code-blocks:*code-blocks* t)
      (s t))
  (format s "~&----~%")
  (with-open-file (s "/tmp/foo.html" :if-exists :supersede
                                     :direction :output)
    (3bmd:parse-string-and-print-to-stream "a footnote[^ref] ref

[^ref]: the definition  
multiline


```
some more body text



a

b

c

d

e

f

f
```

Here's a simple footnote,[^1] and[^1] here[^1]'s a longer one.[^bignote]


[^1]: This is the first footnote.

[^bignote]: Here's one with multiple paragraphs and code.

    Indent paragraphs to include them in the footnote.

    `{ my code }`

    * Add as many paragraphs as you like.
    * liost

" s)))


