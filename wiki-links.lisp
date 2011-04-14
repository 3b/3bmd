(defpackage #:3bmd-wiki
  (:use #:cl #:esrap #:3bmd-ext)
  (:export #:*wiki-links*
           #:*wiki-links*
           #:*wiki-processor*
           #:process-wiki-link
           ))
(in-package #:3bmd-wiki)

;;; example extension for handling wiki-style [[links]]


;;; extending parser:
;;; define independent rules with esrap:defrule then use
;;; define-extension-inline to add the extension to the main grammar

;; allowing markup for now, to be normalized like ref links during printing...
(defrule wiki-link-label (* (and (! #\]) (! #\|) inline))
  (:lambda (a)
    (mapcar 'third a)))

(defrule wiki-link-arg (* (and (! "|") (! "]]") character))
  (:concat t))
(defrule normal-wiki-link (and
                           "[["
                           wiki-link-label
                           (* (and "|" wiki-link-arg))
                           "]]")
  (:destructure ([ label args ])
                (declare (ignore [ ]))
                (list :wiki-link :label label :args (mapcar 'second args))))

(defrule quoted-wiki-link (and #\'
                               ;; would be nicer to just use wiki-link
                               ;; rule rather than duplicating it
                               ;; here, but then we'd have to
                               ;; serialize it back to text to put the
                               ;; "[[" back, and worry about
                               ;; whitespace, etc
                               "[["
                               (* (and (! #\]) character))
                               "]]")
  (:destructure (q &rest link)
                (declare (ignore q))
                (concat link)))

(define-extension-inline *wiki-links* wiki-link
    (or quoted-wiki-link normal-wiki-link)
  (:character-rule wiki-link-extended-chars #\| #\' #\=)
  (:after 3bmd-grammar::emph))



;;; extending printer:
;;; add a method to print-tagged-element specialized for the values
;;;   returned by the new parser rules

(defparameter *wiki-processor* nil
  "set to something PROCESS-WIKI-LINK etc will recognize to enable wiki link support in printer (see also *wiki-links* to enable wiki link parsing)")

(defgeneric process-wiki-link (wiki normalized-target formatted-target args stream)
  ;; just ignore the link by default
  (:method ((w null) nt formatted a stream)
    (declare (ignore w nt a))
    (format stream "~a" formatted)))

(defmethod print-tagged-element ((tag (eql :wiki-link)) stream rest)
  (destructuring-bind (&key label args) rest
    (let ((formatted (with-output-to-string (s)
                       (loop for i in label do (print-element i s))))
          ;; todo: figure out how to normalize formatted links, or
          ;; restrict the grammar to disalow them
          (normalized (print-label-to-string label)))
      (process-wiki-link *wiki-processor* normalized formatted args stream))))


#++
(let ((3bmd-wiki:*wiki-links* t))
  (esrap:parse 'inline "[[foo|bar]]"))

#++
(let ((3bmd-wiki:*wiki-links* t))
  (esrap:parse 'inline "'[[foo|bar]]"))

#++
(let ((3bmd-wiki:*wiki-links* t))
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream "[[foo|bar]]" s)))
