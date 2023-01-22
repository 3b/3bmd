(in-package #:3bmd-grammar)

;;; built-in extension to markdown grammar for smart quotes

;; we store the original text for ellipsis,dashes,etc so we can use it inside
;; code blocks
(defrule ellipsis (or "..." ". . .")
  (:lambda (a) (list :ellipsis a)))

(defrule dash (or em-dash en-dash))
#++
(defrule em-dash (or "---" "--")
  (:lambda (a) (list :em-dash a)))
#++
(defrule en-dash (and "-" (& dec-digit))
  (:lambda (a) (cons :en-dash a)))
;; not sure what the - &digit thing was trying to do, so just doing
;; "--" -> en-dash and "---" -> em-dash instead, and "-" can stay as-is
(defrule em-dash "---"
  (:lambda (a) (list :em-dash a)))
(defrule en-dash "--"
  (:lambda (a) (list :en-dash a)))

(defrule single-quote-start (and #\'
                                 (! #.`(or ,@(coerce ")!],.;:-?" 'list)
                                           #\space #\tab #\newline
                                           #\return))
                                 (! (and (or "s" "t" "m" "ve" "ll" "rr")
                                         (! alphanumeric))))
  (:constant ""))
(defrule single-quote-end (and #\' (! alphanumeric))
  (:constant ""))
(defrule single-quoted (and single-quote-start
                            (+ (and (! single-quote-end)
                                    %inline))
                            single-quote-end)
  (:destructure (q content q2)
                (declare (ignore q q2))
    (cons :single-quoted (mapcar 'second content))))


(defrule double-quote-start #\"
  (:constant ""))
(defrule double-quote-end #\"
  (:constant ""))
(defrule double-quoted (and double-quote-start
                            (+ (and (! double-quote-end)
                                    %inline))
                            double-quote-end)
  (:destructure (q content q2)
                (declare (ignore q q2))
    (cons :double-quoted (mapcar 'second content))))

(defrule apostrophe #\'
  (:constant :apostrophe))

(macrolet ((define-arrows (name pattern)
             `(defrule ,name ,pattern
                (:lambda (a) (list ,(alexandria:make-keyword name) a)))))
  (define-arrows left-right-single-arrow "<->")
  (define-arrows left-single-arrow "<-")
  (define-arrows right-single-arrow "->")
  (define-arrows left-right-double-arrow "<=>")
  (define-arrows left-double-arrow "<=")
  (define-arrows right-double-arrow "=>"))

(defrule arrows (or left-right-single-arrow left-single-arrow right-single-arrow
                    left-right-double-arrow left-double-arrow right-double-arrow))


(define-extension-inline *smart-quotes* smart
    (or ellipsis dash single-quoted double-quoted
        apostrophe
        arrows)
  (:character-rule smart-quote-extended-chars #\. #\- #\' #\" #\=)
  (:escape-char-rule smart-quote-escaped-characters #\" #\' #\=)
  (:after escaped-character))

#++
(let ((*smart-quotes* nil))
  (esrap:parse '%inline "..."))
#++
(esrap:find-rule '%inline)




