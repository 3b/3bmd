(in-package 3bmd-tests)


(def-grammar-test explicit-image-test-1
  :rule 3bmd-grammar::image
  :text "![](http://placekitten.com/300/200)"
  :expected '(:IMAGE
              (:EXPLICIT-LINK
               :LABEL NIL
               :SOURCE "http://placekitten.com/300/200"
               :TITLE NIL)))

(def-grammar-test explicit-image-test-2
  :rule 3bmd-grammar::image
  :text "![A Label](http://placekitten.com/300/200 \"And title\")"
  :expected '(:IMAGE
              (:EXPLICIT-LINK
               :LABEL ("A" " " "Label")
               :SOURCE "http://placekitten.com/300/200"
               :TITLE "And title")))

(def-grammar-test explicit-image-test-3
  :rule 3bmd-grammar::image
  :text "![A Label](http://placekitten.com/300/200 'And title')"
  :expected '(:IMAGE
              (:EXPLICIT-LINK
               :LABEL ("A" " " "Label")
               :SOURCE "http://placekitten.com/300/200"
               :TITLE "And title")))

(def-grammar-test explicit-image-test-4
  :rule 3bmd-grammar::image
  ;; Label can contain another inline elements, but the title is not
  :text "![A **Label**](http://placekitten.com/300/200 'And **title**')"
  :expected '(:IMAGE
              (:EXPLICIT-LINK
               ;; What is interesting, in CommonMark reference
               ;; parser, all formatting from the label is removed
               ;; and it is rendered into the HTML as
               ;; a plaintext IMG's attribute ALT
               :LABEL ("A" " " (:STRONG "Label"))
               :SOURCE "http://placekitten.com/300/200"
               ;; Here 3BMD is inline with the CommonMark
               ;; reference parser
               :TITLE "And **title**")))


(def-grammar-test explicit-image-test-5
  :rule 3bmd-grammar::image
  ;; TODO: CommonMark prohibits multiline image definitions
  ;; and will parse such text as usual paragraph.
  :text "![A Label]
(http://placekitten.com/300/200
'And title')"
  :expected '(:IMAGE
              (:EXPLICIT-LINK
               :LABEL ("A" " " "Label")
               :SOURCE "http://placekitten.com/300/200"
               :TITLE "And title")))


;; Original Markdown definition allows referenced
;; images: https://daringfireball.net/projects/markdown/syntax#img
;; But neither CommonMark nor GitHub flavoured markdown
;; don't support them.

(def-grammar-test reference-image-test-1
  :rule 3bmd-grammar::image
  :text "![A Label][reference]"
  :expected '(:IMAGE
              (:REFERENCE-LINK
               :LABEL ("A" " " "Label")
               :DEFINITION ("reference"))))

(def-grammar-test reference-image-test-2
  :rule 3bmd-grammar::image
  ;; TODO: CommonMark prohibits multiline image definitions
  ;; and will parse such text as usual paragraph.
  :text "![A Label]
[reference]"
  :expected '(:IMAGE
              (:REFERENCE-LINK
               :LABEL ("A" " " "Label")
               :DEFINITION ("reference"))))

(def-grammar-test reference-image-test-3
  :rule 3bmd-grammar::image
  :text "![A Label][]"
  :expected '(:IMAGE
              (:REFERENCE-LINK
               :LABEL ("A" " " "Label")
               ;; Why does not we have a
               ;; :DEFINITION "A Label"
               :TAIL "[]")))

(def-grammar-test reference-image-test-4
  :rule 3bmd-grammar::image
  :text "![A Label]
[]"
  :expected '(:IMAGE
              (:REFERENCE-LINK
               :LABEL ("A" " " "Label")
               ;; Why does not we have a
               ;; :DEFINITION "A Label"
               ;; Also, note the :TAIL has a newline
               ;; in this multiline version
               :TAIL "
[]")))
