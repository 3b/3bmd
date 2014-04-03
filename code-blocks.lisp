(defpackage #:3bmd-code-blocks
  (:use #:cl #:esrap #:3bmd-ext)
  (:export #:*code-blocks*
           #:*code-blocks-default-colorize*
           #:*code-blocks-pre-class*
           #:*code-blocks-span-class*
           #:*colorize-code-spans-as*
           #:*code-blocks-coloring-type-remap*))
(in-package #:3bmd-code-blocks)

;;; github style ``` delimited code blocks, with colorize support

(defparameter *code-blocks-default-colorize* nil
  "a colorize coloring type name, like :common-lisp or :elisp ")
(defparameter *colorize-code-spans-as* nil)
(defparameter *colorize-verbatim-block-as* nil)

;;; allow remapping coloring types
;;;  for example if there is a coloring type ":lisp-with-extra-symbols" defined,
;;;  but the markdown files use "```lisp", bind a hash table with
;;;  key :lisp -> value :lisp-with-extra-symbols while printing
(defparameter *code-blocks-coloring-type-remap* nil
  "bind to a hash table mapping symbols to symbols to remap coloring types.")

(defparameter *code-blocks-pre-class* nil
  "css class to use for <pre> blocks (for ``` blocks)")
(defparameter *code-blocks-span-class* nil
  "css class to use for <span>s from colorized `` inlines")

(defparameter *colorize-name-map*
  ;; names are downcased and whitespace,-,_ removed before looking them up
  (alexandria:plist-hash-table '("lisp" :common-lisp
                                 "basiclisp" :lisp
                                 "scheme" :scheme
                                 "elisp" :elisp
                                 "emacslisp" :elisp
                                 ;; common-lisp-file?
                                 "cl" :common-lisp
                                 "commonlisp" :common-lisp
                                 "clisp" :common-lisp
                                 "c" :c
                                 "c++" :c++
                                 "java" :java
                                 "objc" :objective-c
                                 "objectivec" :objective-c
                                 "erlang" :erlang
                                 "python" :python
                                 "haskell" :haskell
                                 "diff" :diff
                                 "webkit" :webkit)
                               :test #'equal))

(defun find-coloring-type (name)
  (let* ((n (string-downcase (remove-if (lambda (a)
                                          (member a '(#\space #\tab #\newline
                                                      #\return #\_ #\-)
                                                  :test 'char=))
                                        name)))
         (s (gethash n *colorize-name-map*)))
    (or (and *code-blocks-coloring-type-remap*
             (gethash s *code-blocks-coloring-type-remap*))
        s)))


;;; we start with ``` optionally followed by a language name on same line
(defrule code-block-start (and "```" (* (and (! 3bmd-grammar::newline) character)) 3bmd-grammar::newline)
  (:destructure (|`| lang nl)
                (declare (ignore |`| nl))
                (list 'code-block :lang (string-trim (list #\space #\tab) (text lang)))))

;;; and end with ``` on a line by itself
(defrule code-block-end (and 3bmd-grammar::newline
                             "```"
                             (or 3bmd-grammar::newline
                                       3bmd-grammar::eof))
  (:constant nil))

;;; and store anything in between as is
(defrule code-block-content (* (and (! code-block-end)
                                    character))
  (:text t))


(define-extension-block *code-blocks* code-block
    (and code-block-start code-block-content code-block-end)
    ;; 'heading' could misparse a code block that starts with ---- or =====
  (:before 3bmd-grammar::heading)
  (:destructure (s c e)
                (declare (ignore e))
                (append s (list :content c))))

;;; todo: make the CSS class for colorized blocks configurable
(defmethod print-tagged-element ((tag (eql 'code-block)) stream rest)
  (destructuring-bind (&key lang content) rest
    (let* ((clang (or (find-coloring-type lang)
                      *code-blocks-default-colorize*))
           (formatted (if clang
                          (let ((colorize::*css-background-class* "code"))
                            (colorize::html-colorization clang content))
                          content)))
      (3bmd::padded (2 stream)
        (format stream "<pre~@[ class=\"~a\"~]><code>" *code-blocks-pre-class*)
        (format stream "~a" formatted)
        (format stream "</code></pre>")))))

(defmethod print-md-tagged-element ((tag (eql 'code-block)) stream rest)
  (3bmd::ensure-paragraph stream)
  (format stream "```~a~%~a~%```" (getf rest :lang) (getf rest :content))
  (3bmd::end-paragraph stream))


;;; fixme: add hooks to do this properly, so multiple extensions don't conflict
(defmethod print-tagged-element :around ((tag (eql :code)) stream rest)
  (if *colorize-code-spans-as*
      (format stream "~a"
              (let ((colorize::*css-background-class* (or *code-blocks-span-class*
                                                          "code")))
                (colorize::html-colorization *colorize-code-spans-as*
                                             (text rest))))
      (call-next-method)))


#++
(let ((*code-blocks* t))
  (esrap:parse '3bmd-grammar::doc "```scheme
 (define (foo a)
 (+ a 1))
```"))

#++
(let ((*code-blocks* t))
  (esrap:parse '3bmd-grammar::doc "```
 (define (foo a)
 (+ a 1))
```
"))

#++
(let ((*code-blocks* t))
  (esrap:parse '3bmd-grammar::doc "```foo bar baz
 (define (foo a)
 (+ a 1))
```
"
))

#++
(let ((*code-blocks* t))
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream
     "```scheme
 (define (foo a)
 (+ a 1))
```" s)))

#++
(let ((*code-blocks* t))
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream
     "```Common Lisp
 (defun foo (a)
 (+ a 1))
```" s)))

#++
(let ((*code-blocks* t)
      (*colorize-code-spans-as* nil))
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream
     "a `(defun a() )` b" s)))
