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

(def-grammar-test formatted-quote
  :rule 3bmd-grammar::doc
  :text "> *a*
> > ---"
  :expected '((:BLOCK-QUOTE (:PARAGRAPH (:EMPH "a"))
               (:BLOCK-QUOTE (:HORIZONTAL-RULE)))))

(def-grammar-test formatted-quote-2
  :rule 3bmd-grammar::doc
  :text "- 1
    > *a*
    > > ---"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PARAGRAPH "1")
                (:BLOCK-QUOTE
                 (:PARAGRAPH (:EMPH "a"))
                 (:BLOCK-QUOTE
                  (:HORIZONTAL-RULE)))))))


(def-grammar-test nested-block-quote
  :rule 3bmd-grammar::doc
  :text  "- 1
    > 2 3
    >
    > - 4
    >     > 5 6
    >     > 7 8
    > 9
 "
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PARAGRAPH "1")
                (:BLOCK-QUOTE (:PARAGRAPH "2" " " "3")
                 (:BULLET-LIST
                  (:LIST-ITEM (:PARAGRAPH "4")
                   ;; markdown and commonmark seem to agree that the 9
                   ;; is part of the inner quote
                   (:BLOCK-QUOTE
                    (:PARAGRAPH "5" " " "6" "
"
                                "7" " " "8" "
"
                                "9")))))))))
