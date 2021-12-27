(in-package 3bmd-tests)


(def-grammar-test simple-block-quote
  :rule 3bmd-grammar::block-quote
  :text "> Foo
> Bar
> Blah

Just a paragraph"
  :expected '(:BLOCK-QUOTE
              (:PARAGRAPH "Foo" "
"
               "Bar" "
"
               "Blah"))
  :remaining-text "Just a paragraph")


(def-grammar-test block-quote-with-single-starting-symbol
  :rule 3bmd-grammar::block-quote
  :text "> Foo
Bar
Blah
Minor

Just a paragraph"
  :expected '(:BLOCK-QUOTE
              (:PARAGRAPH "Foo" "
"
               "Bar" "
"
               "Blah" "
"
               "Minor"))
  :remaining-text "Just a paragraph")


(def-grammar-test block-quote-inside-a-list
  :rule 3bmd-grammar::doc
  :text "* The list

  > Foo
  > Bar
  > Blah

Just a paragraph"
  :expected '((:BULLET-LIST (:LIST-ITEM (:PLAIN "The" " " "list")))
              ;; TODO:
              ;; Problem 1: The quote is not inside the list item
              ;; Problem 2: The quote is not parsed as a quote
              (:PARAGRAPH ">" " " "Foo" "
"
               "  " ">" " " "Bar" "
"
               "  " ">" " " "Blah")
              ;; Problem 3: Paragraph at the end of the document is not recognized.
              (:PLAIN "Just" " " "a" " " "paragraph")))

