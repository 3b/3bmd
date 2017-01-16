;-------------------------------------------------------------------------------
; Package and generics
;-------------------------------------------------------------------------------
(defpackage #:3bmd-code-blocks
  (:use #:cl #:esrap #:3bmd-ext #:uiop #:split-sequence)
  (:export #:render-code
           #:render-code-block
           #:start-renderer
           #:stop-renderer
           #:renderer-started-p
           #:*code-blocks*
           #:*renderer*
           #:*render-code-spans*
           #:*render-code-spans-lang*
           #:*code-blocks-default-colorize*
           #:*code-blocks-pre-class*
           #:*code-blocks-span-class*
           #:*code-blocks-coloring-type-remap*))

(in-package #:3bmd-code-blocks)

;;; github style ``` delimited code blocks, with colorize support

(defgeneric render-code-block (renderer stream lang params code)
  (:documentation "Render CODE block written in LANG to STREAM"))

(defgeneric render-code (renderer stream code)
  (:documentation "Render CODE written in LANG to STREAM"))

(defvar *renderer-started* nil
  "State of the renderer")

(defgeneric start-concrete-renderer (renderer)
  (:documentation "Start the code renderer")
  (:method (renderer) nil))

(defgeneric stop-concrete-renderer (renderer)
  (:documentation "Stop the code renderer")
  (:method (renderer) nil))

(defun start-renderer ()
  (start-concrete-renderer *renderer*)
  (setf *renderer-started* t))

(defun stop-renderer ()
  (stop-concrete-renderer *renderer*)
  (setf *renderer-started* nil))

(defun renderer-started-p ()
  (eq *renderer-started* t))

(defparameter *renderer* :colorize
  "Select rendering back-end. :colorize and :pygments are implemented by default.")

;; uiop:run-program searches PATH on at least some implementations,
;; may need to specify full path or pass :FORCE-SHELL T to
;; uiop:launch-program if it doesn't on others
(defparameter *python-command* "python3")

(defparameter *render-code-spans* nil
  "Render in-line code spans.")

(defparameter *render-code-spans-lang* nil
  "Default language used in in-line code spans.")

(defvar *pygmentize-path*
  (merge-pathnames "pygmentize.py"
                   #.(or *compile-file-truename* *load-truename*))
  "Path to the pygmentize script")

;-------------------------------------------------------------------------------
; Colorize
;-------------------------------------------------------------------------------
(defparameter *code-blocks-default-colorize* nil
  "a colorize coloring type name, like :common-lisp or :elisp ")
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
                                 "clj" :clojure
                                 "clojure" :clojure
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

;;; todo: make the CSS class for colorized blocks configurable
(defmethod render-code-block ((renderer (eql :colorize)) stream lang params code)
  (let* ((clang (or (find-coloring-type lang)
                    (unless (and lang (string/= lang ""))
                      *code-blocks-default-colorize*)))
         (formatted (if clang
                        (let ((colorize::*css-background-class* "code"))
                          (colorize::html-colorization clang code))
                        (3bmd::escape-pre-string code))))
    (3bmd::padded (2 stream)
      (format stream "<pre~@[ class=\"~a\"~]><code>" *code-blocks-pre-class*)
      (format stream "~a" formatted)
      (format stream "</code></pre>"))))

(defmethod render-code ((renderer (eql :colorize)) stream code)
  (format stream "<code>~a</code>"
          (let ((colorize::*css-background-class* (or *code-blocks-span-class*
                                                      "code")))
            (colorize::html-colorization *render-code-spans-lang* code))))

;-------------------------------------------------------------------------------
; Pygments
;-------------------------------------------------------------------------------
(defvar *pygmentize-process* nil)

(defmethod start-concrete-renderer ((renderer (eql :pygments)))
  (setf *pygmentize-process* (uiop:launch-program
                              (list *python-command*
                                    (namestring *pygmentize-path*))
                              :input :stream
                              :output :stream)))

(defmethod stop-concrete-renderer ((renderer (eql :pygments)))
  (write-line "exit" (process-info-input *pygmentize-process*))
  (force-output  (process-info-input *pygmentize-process*))
  (wait-process *pygmentize-process*))

(defun pygmentize-code (lang params code)
  (let ((proc-input (process-info-input *pygmentize-process*))
        (proc-output (process-info-output *pygmentize-process*)))
    (write-line (format nil "pygmentize|~a|~a~@[|~a~]"
                        (length code) lang params)
                proc-input)
    (write-string code proc-input)
    (force-output proc-input)
    (let ((nchars (parse-integer
                   (nth 1
                        (split-sequence #\| (read-line proc-output))))))
      (coerce (loop repeat nchars
                 for x = (read-char proc-output)
                 collect x)
              'string))))

(defmethod render-code-block ((renderer (eql :pygments)) stream lang params code)
  (let ((started-before (renderer-started-p)))
    (if (not started-before)
        (start-renderer))
    (format stream "~a" (pygmentize-code lang params code))
    (if (not started-before)
        (stop-renderer))))

(defmethod render-code ((renderer (eql :pygments)) stream code)
  (let ((s (make-string-output-stream)))
    (render-code-block renderer s *render-code-spans-lang* "nowrap" code)
    (format stream "<span class=\"highlight\"><code>~a</code></span>"
            (string-right-trim '(#\Newline)
                               (get-output-stream-string s)))))

;-------------------------------------------------------------------------------
; Parsing
;-------------------------------------------------------------------------------
;;; extra parameters to be passed to the renderer
(defrule code-block-params (and "|"
                                (* (and (! 3bmd-grammar::newline) character)))
  (:destructure (vert params)
                (declare (ignore vert))
                (when params (text params))))

;;; we start with ``` optionally followed by a language name on same line
(defrule code-block-start (and "```"
                               (* (and (! 3bmd-grammar::newline) (! "|") character))
                               (? code-block-params)
                               3bmd-grammar::newline)
  (:destructure (|`| lang params nl)
                (declare (ignore |`| nl))
                (list 'code-block
                      :lang (string-trim (list #\space #\tab) (text lang))
                      :params params)))

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

;-------------------------------------------------------------------------------
; Rendering
;-------------------------------------------------------------------------------
(defmethod print-tagged-element ((tag (eql 'code-block)) stream rest)
  (destructuring-bind (&key lang params content) rest
    (render-code-block *renderer* stream lang params content)))

(defmethod print-md-tagged-element ((tag (eql 'code-block)) stream rest)
  (3bmd::ensure-block stream)
  (format stream "```~a~@[|~a~]~%~a~%```"
          (getf rest :lang) (getf rest :params) (getf rest :content))
  (3bmd::end-block stream))

;;; fixme: add hooks to do this properly, so multiple extensions don't conflict
(defmethod print-tagged-element :around ((tag (eql :code)) stream rest)
  (if *render-code-spans*
      (render-code *renderer* stream (text rest))
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
