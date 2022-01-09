(in-package 3bmd-tests)


(def-grammar-test eof-test
  :rule 3bmd-grammar::eof
  :expected ""
  :text "")


(def-grammar-test space-test
  :rule 3bmd-grammar::space-char
  :expected " "
  :text " ")


(def-grammar-test tab-as-space
  :rule 3bmd-grammar::space-char
  :expected "	"
  :text "	")


(def-grammar-test multiple-spaces-mixed-with-tabs
  :rule 3bmd-grammar::sp
  ;; three spaces, tab and one space
  :expected "   	 "
  :text "   	 ")


(def-grammar-test newline
  :rule 3bmd-grammar::newline
  :expected "
"
  :text "
")


(def-grammar-test blank-line-test1
  :rule 3bmd-grammar::blank-line
  :text "
"
    :expected "
")


(def-grammar-test blank-line-test2
  :rule 3bmd-grammar::blank-line
  ;; Hanging space chars should be discarded
  :text "   	 
"
    :expected "
"
)


(def-grammar-test indent-by-tab-should-be-replaced-with-spaces
  :rule 3bmd-grammar::indent
  :expected "    "
  :text "	")


;; For some reason, space number other than 4
;; is considered as non indent in 3bmd.
;; Also, a zero spaces also considered as
;; nonindent "space" :-/
(def-grammar-test non-indent-test-0
  :rule 3bmd-grammar::nonindent-space
  :expected ""
  :text "")

(def-grammar-test non-indent-test-1
  :rule 3bmd-grammar::nonindent-space
  :expected " "
  :text " ")

(def-grammar-test non-indent-test-2
  :rule 3bmd-grammar::nonindent-space
  :expected "  "
  :text "  ")

(def-grammar-test non-indent-test-3
  :rule 3bmd-grammar::nonindent-space
  :expected "   "
  :text "   ")

(def-grammar-test non-indent-test-4-fail
  :rule 3bmd-grammar::nonindent-space
  :fail-expected t
  :text "    ")

;; More than 4 spaces also is not a indent :-/
(def-grammar-test non-indent-test-5-fail
  :rule 3bmd-grammar::nonindent-space
  :fail-expected t
  :text "     ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Endlines ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; There are three types of endlines in 3BMD:
;;
;; Line breaks are can be inserted in the middle of a paragraph
;; to manually break the line. Usually, when rendering to HTML
;; these line breaks are rendered as <br/> tags.
;;
;; Terminal endlines are finish the document, because this rule
;; matches only at the end of the whole document.
;; 
;; Normal endlines are ones that does not followed by a header,
;; blank line or a block quote. Interesting, that 3BMD parser
;; does not consider other block elements problem when parsing
;; an endline. Thus these endlines are separate lines of text
;; in the paragraph.

(def-grammar-test normal-endline
  :rule 3bmd-grammar::endline
  :text "
"
  :expected "
")

(def-grammar-test endline-matches-if-followed-by-usual-line
  :rule 3bmd-grammar::endline
  :text "
foo"
  :expected "
"
  :remaining-text "foo")

(def-grammar-test normal-endline-does-not-match-when-followed-by-empty-line
  :rule 3bmd-grammar::endline
  :text "

"
  :fail-expected 'esrap:esrap-parse-error)

(def-grammar-test normal-endline-does-not-match-when-followed-by-a-block-quote
  :rule 3bmd-grammar::endline
  :text "
> quote
"
  :fail-expected 'esrap:esrap-parse-error)

(def-grammar-test normal-endline-does-not-match-when-followed-by-an-atx-header
  :rule 3bmd-grammar::endline
  :text "
## Some header
"
  :fail-expected 'esrap:esrap-parse-error)

(def-grammar-test normal-endline-does-not-match-when-followed-by-an-usual-header1
  :rule 3bmd-grammar::endline
  :text "
Some header
===========
"
  :fail-expected 'esrap:esrap-parse-error)

(def-grammar-test normal-endline-does-not-match-when-followed-by-an-usual-header2
  :rule 3bmd-grammar::endline
  :text "
Some header
-----------
"
  :fail-expected 'esrap:esrap-parse-error)

(def-grammar-test line-break-if-two-spaces-at-the-end
  :rule 3bmd-grammar::endline
  :text "  
"
  ;; TODO: Probably that is wrong that endline rule might return
  ;; a (:line-break) or a text?
  :expected '(:LINE-BREAK))

;; TODO:
;; There is something I don't understand. In code it is called
;; a TERMINAL-ENDLINE. It is formed from a handing white-space
;; on the last line of the input. Interesting, why do we need
;; a rule for this special case?
(def-grammar-test terminal-endline-test
  :rule 3bmd-grammar::endline
  :text " 
"
  :expected " 
")
