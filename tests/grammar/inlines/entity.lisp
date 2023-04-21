(in-package 3bmd-tests)

(def-grammar-test ascii-entity-test-1
  :rule 3bmd-grammar::inlines
  :text "Hello &lambda; World!"
  :expected '("Hello" " " (:ENTITY "&lambda;") " " "World" "!"))

(def-grammar-test dec-entity-test-1
  :rule 3bmd-grammar::inlines
  :text "Hello &#955; World!"
  :expected '("Hello" " " (:ENTITY "&#955;") " " "World" "!"))

(def-grammar-test hex-entity-test-1
  :rule 3bmd-grammar::inlines
  :text "Hello &#x03BB; World!"
  :expected '("Hello" " " (:ENTITY "&#x03BB;") " " "World" "!"))


(def-grammar-test nested-entity-test-1
  :rule 3bmd-grammar::doc
  :text "* List item

  &lambda; is great!
"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "List" " " "item")))
              ;; TODO:
              ;; This should be parsed as the second paragraph
              ;; of the first list-item:
              (:PLAIN "  " (:ENTITY "&lambda;") " " "is" " "
               "great" "!")))
