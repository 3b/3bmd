;-------------------------------------------------------------------------------
; Support math markup using libraries like MathJax
; Author: Lukasz Janyst <lukasz@jany.st>
;
; Works both with inline math:
;
; Begining of the paragraph $$ \sum_{i=0}^{10} (u_{i} x_{i})^2 $$ blah blah
;
; and with blocks:
;
; $$
; \sum_{i=0}^{10} (u_{i} x_{i})^2
; $$
;
; Note that this departs from normal TeX syntax, which uses a single $
; for inline math, and double for display math. By using double $, the
; need for escaping $ is much less. Also, although it's not
; documented, GitHub Flavored Markdown supports $$-delimited _inline_
; math, too.
;
;-------------------------------------------------------------------------------

(defpackage #:3bmd-math
  (:use #:cl #:esrap #:3bmd-ext)
  (:export #:*math*
           #:*html-inline-start-marker*
           #:*html-inline-end-marker*
           #:*html-block-start-marker*
           #:*html-block-end-marker*))

(in-package #:3bmd-math)

(defvar *html-inline-start-marker* "\\(")
(defvar *html-inline-end-marker* "\\)")
(defvar *html-block-start-marker* "\\[")
(defvar *html-block-end-marker* "\\]")

(defrule math-content (* (and (! "$$") character))
  (:text t))

(define-extension-inline *math* math-inline
    (and "$$" math-content "$$")
  (:destructure (s c e)
                (declare (ignore s e))
                (list :math-inline c)))

(define-extension-block *math* math-block
    (and "$$" math-content "$$")
    (:destructure (s c e)
                (declare (ignore s e))
                (list :math-block c)))

(defmethod print-tagged-element ((tag (eql :math-inline)) stream rest)
  (format stream "~a~a~a" *html-inline-start-marker* (car rest)
          *html-inline-end-marker*))

(defmethod print-tagged-element ((tag (eql :math-block)) stream rest)
  (format stream "~a~a~a" *html-block-start-marker* (car rest)
          *html-block-end-marker*))

(defmethod print-md-tagged-element ((tag (eql :math-inline)) stream rest)
  (format stream "$$~a$$" (car rest)))

(defmethod print-md-tagged-element ((tag (eql :math-block)) stream rest)
  (3bmd::ensure-block stream)
  (3bmd::print-md (format nil "$$~a$$" (car rest)) stream)
  (3bmd::end-block stream))

#++
(let ((3bmd-math:*math* t))
  (esrap:parse '%inline "$$ \sum_{i=0}^{10} (u_{i} x_{i})^2 $$"))

#++(let ((3bmd-math:*math* t))
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream "test $$ \sum_{i=0}^{10} (u_{i} x_{i})^2 $$ test" s)))

#++(let ((3bmd-math:*math* t))
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream "$$ \sum_{i=0}^{10} (u_{i} x_{i})^2 $$" s)))
