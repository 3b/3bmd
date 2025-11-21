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

;;; ![^foo] parses as an image link, which is assumed to be incorrect
;;; when footnotes are enabled. Add a rule to parse the ! as text in
;;; that case so the [^foo] can be parsed as a footnote.
(define-extension-inline *footnotes* footnote-not-an-image
    (and "!" (& footnote))
  (:before 3bmd-grammar::image)
  (:function first)
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

(defrule footnote-block-line (and (! (or 3bmd-grammar::blank-line
                                         3bmd-grammar::horizontal-rule
                                         (and footnote #\:)))
                                  3bmd-grammar::optionally-indented-line)
  (:function second))


;; hash table of ID -> (ref-name . backref-names)
(defvar *used-footnotes*)
(defvar *expanded-footnotes*)
(defvar *next-ref*)

(define-extension-block *footnotes* footnote-def
    (and (and footnote #\:)
         footnote-block
         (* footnote-continuation-block))
  (:before 3bmd-grammar::reference)
  (:bind *used-footnotes* (make-hash-table :test 'equalp)
         *expanded-footnotes* (make-hash-table :test 'equalp)
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

(defun walk-def (def)
  ;; getting footnotes inside footnotes right is messy:
  ;;
  ;;   we need to include backlinks, even from footnotes we haven't
  ;;   printed yet
  ;;
  ;;   we don't want backlinks to footnotes that get dropped
  ;;
  ;;   footnotes only used from unused footnotes should be considered
  ;;   unused
  ;;
  ;;   not sure if otherwise unused recursive footnotes should be
  ;;   included. I think things are easier if we only consider roots
  ;;   outside defs, so going with that for now.
  ;;
  ;;   if a footnote has a (new) footnote, we probably want that
  ;;   footnote to show up next rather than being added to the end?
  ;;
  ;; so when printing, recursively partially expand the referenced
  ;; definition if it hasn't already been expanded. We can't fully
  ;; print it since we don't have all the backlinks yet, so just
  ;; replace any (footnote-ref x) in the body of the definition with
  ;; the printed representation
  (labels ((expand-def (def)
             (typecase def
               ((cons (eql footnote-ref))
                (list 'expanded-ref
                      (print-tagged-element 'footnote-ref nil (cdr def))))
               (cons
                ;; assuming these are small enough that some extra
                ;; consing won't matter, other option is to walk twice
                ;; to see if we need to expand it?
                (mapcar #'expand-def def))
               (t def))))
    (let ((orig (gethash (list 'footnote-def def) 3bmd::*references*)))
      (when (and orig (not (gethash def *expanded-footnotes*)))
        ;; store something before walking contents so we don't get
        ;; stuck in a loop if there is a recursive footnote
        (setf (gethash def *expanded-footnotes*) :processing)
        (setf (gethash def *expanded-footnotes*)
              (expand-def orig))))))

(defmethod print-tagged-element ((tag (eql 'footnote-ref)) stream rest)
  (let* ((id (car rest))
         (defined (gethash (list 'footnote-def id) 3bmd::*references*)))
    (cond
      ((not defined)
       (format stream "[^~a]" id))
      (t
       (let* ((use (gethash id *used-footnotes*))
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
         (walk-def id)
         (format stream "<sup><a href=\"#~a\" id=\"~a\" >~a</a></sup>"
                 fn back
                 refno))))))

(defmethod print-tagged-element ((tag (eql 'footnote-def)) stream rest)
  ;; definitions will be printed in the footer, so ignore them here
  )

(defmethod print-tagged-element ((tag (eql 'expanded-ref)) stream rest)
  (format stream "~a" (car rest)))

(defvar *backlinks* nil)
(defmethod print-tagged-element ((tag (eql 'footnote-backlinks)) stream rest)
  (loop for b in *backlinks*
        do (format stream " <a href=\"#~a\" class=\"footnote-back\">↩︎</a>" b)))

(defmethod print-def (s refs (format (eql :html)))
  (let ((def (gethash (first refs) *expanded-footnotes*))
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


(defmethod 3bmd::print-md-tagged-element ((tag (eql 'footnote-ref)) stream rest)
  (format stream "[^~a]" (first rest)))

(defmethod 3bmd::print-md-tagged-element ((tag (eql 'footnote-def)) stream rest)
  (3bmd::ensure-block stream)
  (format stream "~&[^~a]: " (first rest))
  (3bmd::with-md-indent (4)
    (loop for i in (cdr rest)
          do (3bmd::print-md-element i stream)))
  (3bmd::end-block stream))

(defmethod 3bmd::print-md-tagged-element ((tag (eql 'footnote-backlinks)) stream rest)
  )


(3bmd::pprinter footnote-def (s o)
  (format s "[^~a]: ~{~a ~}" (cadr o) (cddr o)))

(3bmd::pprinter footnote-ref (s o)
  (format s "[^~a]" (cadr o)))

(3bmd::pprinter footnote-backlinks (s o)
  (declare (ignore s o)))

#++
(let ((*footnotes* t)
      (3bmd-code-blocks:*code-blocks* t)
      (3bmd-tables:*tables* t)
      (s t))
  (format s "~&----~%")
  (with-open-file (s "/tmp/foo.html" :if-exists :supersede
                                     :direction :output)
    (3bmd:parse-string-and-print-to-stream "a footnote[^ref] ref

[^ref]: the definition  
[^a] multiline

A footnote in a paragraph[^1]

| Column1   | Column2[^1] |
| --------- | ------- |
| foot [^a] | note    |

[^a]: a footnote

```
some more body text


a[^a]

b

c

d

e

f

f
```

aHere's a simple footnote,[^1][^bignote] and[^1] here[^1]'s a longer one![^bignote]


[^1]: This is the first footnote[^indirect].

[^unused]: unused footnote[^double-unused]

[^indirect]: indirect footnote

[^double-unused]: unused footnote with a ref from unused footnote

[^unused-recursive]: unused recursive footnote[^unused-recursive]

[^umr1]: unused mutually recursive footnote1[^umr2]

[^umr2]: unused mutually recursive footnote2[^umr1]

[^mr1]: mutually recursive footnote1[^mr2]

[^mr2]: mutually recursive footnote2[^mr1]

[^c1]: footnotes without separating lines1
[^c2]: footnotes without separating lines2

[url1]: http://example.com

[^bignote]: Here's one with multiple paragraphs and code. missing def[^missing]

    Indent paragraphs to include them in the footnote[^mr2].

    `{ my code }` [a [^c1] b](http://example.com) [c d](http://example.com)

    * Add as many paragraphs as you like[^c1].
    * list[^c2][^nested]
    * [ref url][url1]
    * [nested ref url][url2]

    [^nested]: probably doesn't work?

    [url1]: http://example.com

    more text

end of the body text...

" s :format :html)))
