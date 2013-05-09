(defpackage #:3bmd-definition-lists
  (:use #:cl #:esrap #:3bmd-ext)
  (:export #:*definition-lists*
           ))
(in-package #:3bmd-definition-lists)

;;; PHP Markdown Extra style definition lists

;;; definition is:
;;;   optional blank lines
;;;   optional non-indent spaces #\: spaces ...
;;;
;;;   if blank line before def, wrap def in paragraph
;;     (php markdown extra
;;     (multimarkdown,maruku wraps all entries in P if any blank lines
(defrule definition-marker (and 3bmd-grammar::nonindent-space
                                #\: (+ 3bmd-grammar::space-char))
  (:constant :definition-list-const))

;; fixme: figure out how to share code with bullet/enumerated lists?
(defrule definition-definition (and (& (and (? 3bmd-grammar::blank-line)
                                            definition-marker))
                                    definition-definition-list
                                    #++(or definition-list-tight
                                        definition-list-loose))
  (:destructure (a b)
    (declare (ignore a))
    b))


(defrule definition-definition-list (and (+ definition-list-item)
                                         (* 3bmd-grammar::blank-line)
                                         (! definition-marker))
  (:destructure (items i1 i2)
    (declare (ignore i1 i2))
    items))


(defrule definition-list-item (and (? 3bmd-grammar::blank-line)
                                     definition-marker
                                     definition-list-block
                                     (* definition-list-continuation-block))
  (:destructure (blank b block cont)
    (declare (ignore b ))
    (list* 'definition-list-item
           (mapcan (lambda (a)
                     (3bmd-grammar::parse-doc (text a)))
                   (split-sequence:split-sequence
                    :split  (list* block blank (mapcan 'identity cont))
                    :remove-empty-subseqs t)))))



(defrule definition-list-block (and (! 3bmd-grammar::blank-line)
                                    3bmd-grammar::line
                                    (* definition-list-block-line))
  (:destructure (b l block)
                (declare (ignore b))
                (text l block)))

(defrule definition-list-continuation-block (and (* 3bmd-grammar::blank-line)
                                                 (+ (and 3bmd-grammar::indent
                                                         definition-list-block)))
  (:destructure (b c)
                (if b
                    (cons (text b) (mapcar 'second c))
                    (cons :split (mapcar 'second c)))))

(defrule definition-list-block-line (and (! 3bmd-grammar::blank-line)
                                        (! (and (? 3bmd-grammar::indent)
                                                definition-marker))
                              (! 3bmd-grammar::horizontal-rule)
                              3bmd-grammar::optionally-indented-line)
  (:destructure (i1 i2 i3 line)
    (declare (ignore i1 i2 i3))
    line))


(defrule definition-terms (and 3bmd-grammar::nonindent-space
                               (! definition-marker)
                               definition-term-inlines)
  (:destructure (space x terms)
              (declare (ignore space x))
    (mapcar (lambda (a) (cons 'definition-term a)) terms)))

(defrule definition-endline (or 3bmd-grammar::line-break
                                3bmd-grammar::normal-endline
                                (and 3bmd-grammar::sp
                                     3bmd-grammar::newline
                                     (& 3bmd-grammar::blank-line)))
  (:text t))

(defrule definition-term-inlines (+ (and (! definition-marker)
                                         (+ (and (! 3bmd-grammar::endline)
                                                 3bmd-grammar::inline))
                                         definition-endline))
  (:destructure (&rest terms)
    (mapcar (lambda (a) (mapcar (lambda (b)
                                  (or (first b) (second b)))
                                (second a)))
            terms)))




(define-extension-block *definition-lists* definition-list
    (+ (and definition-terms
            definition-definition))
  (:before 3bmd-grammar::paragraph)
  (:destructure (&rest definitions)
                (list* 'definition-list
                       (mapcar (lambda (a)
                                 (list :terms (first a)
                                       :definitions (second a)))
                               definitions))))

(defmethod print-tagged-element ((tag (eql 'definition-list)) stream rest)
  (3bmd::padded (2 stream)
    (format stream "<dl>~%"))
  (loop for def in rest
        do (destructuring-bind (&key terms definitions) def
             (mapcar (lambda (a) (print-element a stream)) terms)
             (mapcar (lambda (a) (print-element a stream)) definitions)))
  (3bmd::padded (1 stream)
    (format stream "~&</dl>")))

(defmethod print-tagged-element ((tag (eql 'definition-term)) stream rest)
  (3bmd::padded (1 stream 2)
    (format stream "<dt>"))
  (mapcar (lambda (a) (print-element a stream)) rest)
  (format stream "</dt>")
  (setf 3bmd::*padding* 0))

(defmethod print-tagged-element ((tag (eql 'definition-list-item)) stream rest)
  (3bmd::padded (1 stream 2)
    (format stream "<dd>"))
  (mapcar (lambda (a) (print-element a stream)) rest)
  (format stream "</dd>")
  (setf 3bmd::*padding* 0))



#++
(let ((*definition-lists* t))
  (esrap:parse '3bmd-grammar::doc "term1 `q`
term2
:  def
  def continued
"))


#++
(let ((*definition-lists* t))
  (esrap:parse '3bmd-grammar::doc "term1
:   definition 1
    def 1 cont

term 2
:   definition 2

"))



#++
(let ((*definition-lists* t))
  (esrap:parse '3bmd-grammar::doc "term1
:   definition 1
def 1 continued

term2
:   definition 2

"))

#++
(let ((*definition-lists* t))
  (esrap:parse '3bmd-grammar::doc "term1
:   definition 1
    definition 1 continues
:   alternate definition 1

term 2
:   definition 2

"))


#++
(let ((*definition-lists* t))
  (esrap:parse '3bmd-grammar::doc "term 1
term 2
:   definition 1,2

term 3
:   definition 3


"))


#++
(let ((*definition-lists* t))
  (esrap:parse '3bmd-grammar::doc
               "t1

: d1

: d2

: d3

: d4
: d5
: d6

t2

: d21
: d22
: d23

: d24
"))



#++
(let ((*definition-lists* t))
  (esrap:parse '3bmd-grammar::doc "`abc`
b

: a

: b
: c

: d

c `q` d
e f
: g
: h
: j

* a
"))


#++
(with-output-to-string (s)
  (let ((*definition-lists* t))
    (3bmd:parse-string-and-print-to-stream "abc
b

: a

: b
: c

: d

c `q` d
e f
: g
: h
: j


nest
: foo
: bar

    hoge
    : piyo
" s)))

