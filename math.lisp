;-------------------------------------------------------------------------------
; Support math markup using libraries like MathJax
; Author: Lukasz Janyst <lukasz@jany.st>
;
; Works with inline math:
;
;     $x_0$
;     $`x_0`$
;     $$x_0$$ text
;
; and block (display) math:
;
;     $$x_0$$
;
; - To avoid rendering "between $5 and $6" with inline math, both the
;   opening and the closing $ character must be followed / preceded by
;   a non-space character. This agrees with Pandoc. The other forms do
;   not have such restriction.
;
; - In the block format, the opening $$ can only be preceded by
;   spaces, and the closing $$ can only be followed by spaces on its
;   own line.
;
; TODO:
;
; - Escaping within math (of e.g. $ characters) is not implemented.
;
;-------------------------------------------------------------------------------

(defpackage #:3bmd-math
  (:use #:cl #:esrap #:3bmd-ext)
  (:import-from #:3bmd #:ensure-block #:end-block #:print-md)
  (:import-from #:3bmd-grammar #:eof #:escaped-character #:newline
                #:sp #:space-char)
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

(define-extension-inline *math* math-inline-1
    (and "$" inline-math-content-1 "$")
  (:character-rule math-extended-chars #\$)
  (:escape-char-rule math-escaped-characters #\$)
  (:md-chars-to-escape #\$)
  (:after escaped-character)
  (:destructure (s c e)
                (declare (ignore s e))
                (list :math-inline-1 c)))

(defrule inline-math-content-1
    (and (! space-char)
         (* (and (* (and (! space-char) (! "$") character))
                 space-char))
         (+ (and (! (or space-char "$")) character)))
  (:text t))

(define-extension-inline *math* math-inline-2
    (and "$`" inline-math-content-2 "`$")
  (:destructure (s c e)
                (declare (ignore s e))
                (list :math-inline-2 c)))

(defrule inline-math-content-2 (* (and (! "`$") character))
  (:text t))

(define-extension-inline *math* math-inline-3
    (and "$$" inline-math-content-3 "$$")
  (:destructure (s c e)
                (declare (ignore s e))
                (list :math-inline-3 c)))

(defrule inline-math-content-3 (* (and (! "$$") character))
  (:text t))

(define-extension-block *math* math-block
    (and "$$" block-math-content "$$" sp (or newline eof))
    (:destructure (s c e sp l)
                (declare (ignore s e sp l))
                (list :math-block c)))

(defrule block-math-content (* (and (! "$$") character))
  (:text t))

(defmethod print-tagged-element ((tag (eql :math-inline-1)) stream rest)
  (format stream "~a~a~a" *html-inline-start-marker* (car rest)
          *html-inline-end-marker*))

(defmethod print-tagged-element ((tag (eql :math-inline-2)) stream rest)
  (format stream "~a~a~a" *html-inline-start-marker* (car rest)
          *html-inline-end-marker*))

(defmethod print-tagged-element ((tag (eql :math-inline-3)) stream rest)
  (format stream "~a~a~a" *html-inline-start-marker* (car rest)
          *html-inline-end-marker*))

(defmethod print-tagged-element ((tag (eql :math-block)) stream rest)
  (format stream "~a~a~a" *html-block-start-marker* (car rest)
          *html-block-end-marker*))

(defmethod print-md-tagged-element ((tag (eql :math-inline-1)) stream rest)
  (format stream "$~a$" (car rest)))

(defmethod print-md-tagged-element ((tag (eql :math-inline-2)) stream rest)
  (format stream "$`~a`$" (car rest)))

(defmethod print-md-tagged-element ((tag (eql :math-inline-3)) stream rest)
  (format stream "$$~a$$" (car rest)))

(defmethod print-md-tagged-element ((tag (eql :math-block)) stream rest)
  (ensure-block stream)
  (print-md (format nil "$$~a$$" (car rest)) stream)
  (end-block stream))
