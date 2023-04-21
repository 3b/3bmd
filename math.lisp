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
;-------------------------------------------------------------------------------

(defpackage #:3bmd-math
  (:use #:cl #:esrap #:3bmd-ext)
  (:export #:*math*))

(in-package #:3bmd-math)

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
  (format stream "\\(~a\\)" (car rest)))

(defmethod print-tagged-element ((tag (eql :math-block)) stream rest)
  (format stream "\\[~a\\]" (car rest)))

#++
(let ((3bmd-math:*math* t))
  (esrap:parse '%inline "$$ \sum_{i=0}^{10} (u_{i} x_{i})^2 $$"))

#++(let ((3bmd-math:*math* t))
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream "test $$ \sum_{i=0}^{10} (u_{i} x_{i})^2 $$ test" s)))

#++(let ((3bmd-math:*math* t))
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream "$$ \sum_{i=0}^{10} (u_{i} x_{i})^2 $$" s)))
