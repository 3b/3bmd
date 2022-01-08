(in-package 3bmd-tests)


(def-grammar-test parse-paragraphs-with-plain-text
  :text "Foo bar.
Second line.

Next paragraph.
"
  :expected '((:PARAGRAPH "Foo" " " "bar." "
"
               "Second" " " "line.")
              ;; TODO: Seems this is wrong and PLAIN
              ;; should be a PARAGRAPH:
              (:PLAIN "Next" " " "paragraph.")))


(def-grammar-test parse-paragraphs-with-plain-text-2
  ;; This example is very much like the previos, but
  ;; contains a blank line after the last paragraph
  :text "Foo bar.
Second line.

Next paragraph.

"
  :expected '((:PARAGRAPH "Foo" " " "bar." "
"
               "Second" " " "line.")
              (:PARAGRAPH "Next" " " "paragraph.")))


(def-grammar-test parse-paragraphs-with-inlines
  :text "Foo `bar` ![](http://some.com/image)
Second *line*.

Next [paragraph][ref].
"
  :expected '((:PARAGRAPH
               "Foo"
               " "
               (:CODE "bar")
               " "
               (:IMAGE
                (:EXPLICIT-LINK :LABEL NIL :SOURCE "http://some.com/image" :TITLE NIL))
               "
"
               "Second"
               " "
               (:EMPH "line")
               ".")
              ;; TODO: Again, this should be a paragraph:
              (:PLAIN
               "Next"
               " "
               (:REFERENCE-LINK :LABEL ("paragraph") :DEFINITION "ref")
               ".")))
