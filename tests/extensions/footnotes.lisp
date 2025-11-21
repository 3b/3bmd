(fiasco:define-test-package #:3bmd-footnotes-tests
  (:use #:3bmd-tests)
  (:import-from #:3bmd-footnotes
                #:footnote
                #:*footnotes*
                #:footnote-ref
                #:footnote-def
                #:footnote-backlinks))

(in-package #:3bmd-footnotes-tests)

(3bmd-tests::def-grammar-test footnote-ref1
  :enable-extensions *footnotes*
  :rule 3bmd-grammar::%block
  :text "a[^ref] reference
"
  :expected '(:plain "a" (footnote-ref "ref")
              " " "reference"))

(3bmd-tests::def-grammar-test footnote-def1
  :enable-extensions *footnotes*
  :rule 3bmd-grammar::%block
  :text "[^def]: definition
"
  :expected '(footnote-def "def"
              (:paragraph "definition"
               (footnote-backlinks))))

(3bmd-tests::def-grammar-test footnote-def2
  :enable-extensions *footnotes*
  :rule 3bmd-grammar::%block
  :text "[^2]: def

    with paragraphs

    ```
    and code
    ```
"
  :expected '(footnote-def "2"
              (:paragraph "def")
              (:paragraph "with" " " "paragraphs")
              (:paragraph (:code "
and code
")
               (footnote-backlinks))))


(3bmd-tests::def-grammar-test footnote-def3
  ;; backlinks go after last child if it isn't a :paragraph
  :enable-extensions *footnotes*
  :rule 3bmd-grammar::%block
  :text "[^2]: def

    with paragraphs

    * 1
    * 2
"
  :expected '(footnote-def "2"
              (:paragraph "def")
              (:paragraph "with" " " "paragraphs")
              (:bullet-list
               (:list-item (:plain "1"))
               (:list-item (:plain "2")))
              (footnote-backlinks)))

(3bmd-tests::def-grammar-test footnote-def4
  :enable-extensions *footnotes*
  :rule 3bmd-grammar::%block
  :text "[^1 2]: not def
"
  :expected '(:plain
              (:reference-link :label ("^1" " " "2")
                               :tail NIL)
              ":" " " "not" " " "def"))


(3bmd-tests::def-grammar-test footnote-def5
  :enable-extensions *footnotes*
  :rule 3bmd-grammar::%block
  :text "[^long-id]: def
"
  :expected '(footnote-def "long-id"
              (:paragraph "def"
               (footnote-backlinks))))

(3bmd-tests::def-grammar-test footnote-def6
  ;; should parse as 2 definitions
  :enable-extensions *footnotes*
  :text "[^1]: def1
[^2]: def2
"
  :expected '((footnote-def "1"
               (:paragraph "def1"
                (footnote-backlinks)))
              (footnote-def "2"
               (:paragraph "def2"
                (footnote-backlinks)))))


(3bmd-tests::def-grammar-test footnote-recursive1
  :enable-extensions *footnotes*
  :rule 3bmd-grammar::%block
  :text "[^def]: an inline[^def] reference in a def
"
  :expected '(footnote-def "def"
              (:paragraph "an" " " "inline"
               (footnote-ref "def")
               " " "reference" " " "in" " " "a" " "
               "def"
               (footnote-backlinks))))

;; should generate a footnote #1, with superscript ¹ linking to
;; "#fn:ref" with id "fnref:ref", and ordered list of definitions
;; in a div with class "footnotes" or similar after all body text
;;
;; possibly add a "see footnote" title?
(def-print-test print-footnotes-1
  :enable-extensions *footnotes*
  :text "a footnote[^ref] ref

[^ref]: the definition

some more body text
"
  :expected "<p>a footnote<sup><a href=\"#fn-1\" id=\"fnref-1\" >1</a></sup> ref</p>
some more body text<section \"id=footnotes\" class=\"footnotes\">

<hr /><ol><li id=\"fn-1\"><p>the definition <a href=\"#fnref-1\" class=\"footnote-back\">↩︎</a></p></li></ol></section>
"
)

(def-print-test print-footnotes-2
  :enable-extensions *footnotes*
  :text "[^def]: a recursive[^def] footnote[^def2]

[^def2]: a normal footnote

body[^def] 2
"
  :expected "body<sup><a href=\"#fn-1\" id=\"fnref-1\" >1</a></sup> 2<section \"id=footnotes\" class=\"footnotes\">

<hr /><ol><li id=\"fn-1\"><p>a recursive<sup><a href=\"#fn-1\" id=\"fnref-1.2\" >1</a></sup> footnote<sup><a href=\"#fn-2\" id=\"fnref-2\" >2</a></sup> <a href=\"#fnref-1\" class=\"footnote-back\">↩︎</a> <a href=\"#fnref-1.2\" class=\"footnote-back\">↩︎</a></p></li><li id=\"fn-2\">

<p>a normal footnote <a href=\"#fnref-2\" class=\"footnote-back\">↩︎</a></p></li></ol></section>
")

;; for multiple references, include multiple backlinks (to distinguish
;; them if keeping original ID in names, add a number after fnref, so
;; "fnref:1" "fnref2:1" "fnref3:1". Other options are just generating
;; sequential numbers for the links and ignoring the supplied ID, or
(def-print-test print-footnotes-3
  :enable-extensions *footnotes*
  :text "multiple[^1] references[^1] to same[^1] footnote

[^1]: a footnote
"
  :expected "<p>multiple<sup><a href=\"#fn-1\" id=\"fnref-1\" >1</a></sup> references<sup><a href=\"#fn-1\" id=\"fnref-1.2\" >1</a></sup> to same<sup><a href=\"#fn-1\" id=\"fnref-1.3\" >1</a></sup> footnote</p><section \"id=footnotes\" class=\"footnotes\">

<hr /><ol><li id=\"fn-1\"><p>a footnote <a href=\"#fnref-1\" class=\"footnote-back\">↩︎</a> <a href=\"#fnref-1.2\" class=\"footnote-back\">↩︎</a> <a href=\"#fnref-1.3\" class=\"footnote-back\">↩︎</a></p></li></ol></section>
")

;; not sure if this should error or merge the definitions or what?
;; GFM takes first definition, and looks like that's what we ended up
;; with too, so good enough.
(def-print-test print-footnotes-4
  :enable-extensions *footnotes*
  :text " a reference[^def]

[^def]: definition

[^def]: a duplicate definition
"
  :expected "<p>a reference<sup><a href=\"#fn-1\" id=\"fnref-1\" >1</a></sup></p><section \"id=footnotes\" class=\"footnotes\">

<hr /><ol><li id=\"fn-1\"><p>definition <a href=\"#fnref-1\" class=\"footnote-back\">↩︎</a></p></li></ol></section>
")

;; drop unused footnotes
(def-print-test print-footnotes-5
  :enable-extensions *footnotes*
  :text " a reference[^1]

[^1]: definition

[^2]: an unused definition
"
  :expected "<p>a reference<sup><a href=\"#fn-1\" id=\"fnref-1\" >1</a></sup></p><section \"id=footnotes\" class=\"footnotes\">

<hr /><ol><li id=\"fn-1\"><p>definition <a href=\"#fnref-1\" class=\"footnote-back\">↩︎</a></p></li></ol></section>
")


;;; footnotes should be appended and numbered in order of use
(def-print-test print-footnotes-6
  :enable-extensions *footnotes*
  :text " multiple[^3] unordered[^1] footnotes[^2]

[^2]: first definition (id 2) third use

[^3]: second definition (id 3) first use

[^1]: third definition (id 1) second use

more content

"
  :expected "<p>multiple<sup><a href=\"#fn-1\" id=\"fnref-1\" >1</a></sup> unordered<sup><a href=\"#fn-2\" id=\"fnref-2\" >2</a></sup> footnotes<sup><a href=\"#fn-3\" id=\"fnref-3\" >3</a></sup></p>

<p>more content</p><section \"id=footnotes\" class=\"footnotes\">

<hr /><ol><li id=\"fn-1\"><p>second definition (id 3) first use <a href=\"#fnref-1\" class=\"footnote-back\">↩︎</a></p></li><li id=\"fn-2\">

<p>third definition (id 1) second use <a href=\"#fnref-2\" class=\"footnote-back\">↩︎</a></p></li><li id=\"fn-3\">

<p>first definition (id 2) third use <a href=\"#fnref-3\" class=\"footnote-back\">↩︎</a></p></li></ol></section>
")

;;; not sure about missing def: some drop completely, some print the
;;; original text of the ref ("[^1]") as if it were not parsed, some
;;; add a footnote with ID as contents
;;
;;; probably just convert back to [^1] in text
(def-print-test print-footnotes-7
  :enable-extensions *footnotes*
  :text " a bad reference[^1] a normal reference[^2]

[^2]: a definition

body
"
  :expected "<p>a bad reference[^1] a normal reference<sup><a href=\"#fn-1\" id=\"fnref-1\" >1</a></sup></p>
body<section \"id=footnotes\" class=\"footnotes\">

<hr /><ol><li id=\"fn-1\"><p>a definition <a href=\"#fnref-1\" class=\"footnote-back\">↩︎</a></p></li></ol></section>
")

;;; make sure we don't generate nested <a> tags. Possibly should parse
;;; as a link with "a [^1] b" as the text instead, or try to split the
;;; link so "a "," b" point to url, and "¹" points to footnote?
;;
;;; fixme: should probably auto-link the URL if not parsing whole
;;; thing as a link?
(def-print-test print-footnotes-8
  :enable-extensions *footnotes*
  :text " [a [^1] b](http://example.com)

[^1]: def

body
"
  :expected "<p>[a <sup><a href=\"#fn-1\" id=\"fnref-1\" >1</a></sup> b](http://example.com)</p>
body<section \"id=footnotes\" class=\"footnotes\">

<hr /><ol><li id=\"fn-1\"><p>def <a href=\"#fnref-1\" class=\"footnote-back\">↩︎</a></p></li></ol></section>
")
