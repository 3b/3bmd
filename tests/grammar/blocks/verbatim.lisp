(in-package 3bmd-tests)


(def-grammar-test spaced-verbatim-block
  :rule 3bmd-grammar::verbatim
  :text "    Foo
    Bar

Just a paragraph"
  :expected '(:VERBATIM "Foo
Bar
")
  :remaining-text "
Just a paragraph")


(def-grammar-test tabbed-verbatim-block
  :rule 3bmd-grammar::verbatim
  :text "	Foo
	Bar

Just a paragraph"
  :expected '(:VERBATIM "Foo
Bar
")
  :remaining-text "
Just a paragraph")


(def-grammar-test verbatim-block-inside-a-list
  :rule 3bmd-grammar::doc
  :text "* List item
      Foo
      Bar

Just a paragraph"
  :expected '((:BULLET-LIST
               (:LIST-ITEM
                ;; TODO:
                ;; Verbatim is not recognized!
                (:PLAIN "List" " " "item" "
"
                 "  " "Foo" "
"
                 "  " "Bar")))
              (:PLAIN "Just" " " "a" " " "paragraph")))

(def-grammar-test verbatim-block-inside-a-list-but-separated-by-empty-line
  :rule 3bmd-grammar::doc
  :text "* List item

      Foo
      Bar

Just a paragraph"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PARAGRAPH "List" " " "item")
                ;; TODO:
                ;; Verbatim is not recognized!
                (:PARAGRAPH "Foo" "
"
                 "  " "Bar")))
              (:PLAIN "Just" " " "a" " " "paragraph")))
