(in-package #:3bmd-tests)


(def-grammar-test horizontal-rule-test-1
  :rule 3bmd-grammar::horizontal-rule
  :text "***

"
  :expected '(:HORIZONTAL-RULE))

(def-grammar-test horizontal-rule-test-2
  :rule 3bmd-grammar::horizontal-rule
  :text "---

"
  :expected '(:HORIZONTAL-RULE))

(def-grammar-test horizontal-rule-test-3
  :rule 3bmd-grammar::horizontal-rule
  :text "___

"
  :expected '(:HORIZONTAL-RULE))

(def-grammar-test horizontal-rule-test-4
  :rule 3bmd-grammar::horizontal-rule
  :text "* * *

"
  :expected '(:HORIZONTAL-RULE))

(def-grammar-test horizontal-rule-test-5
  :rule 3bmd-grammar::horizontal-rule
  :text "****************

"
  :expected '(:HORIZONTAL-RULE))


(def-grammar-test horizontal-rule-nested-in-a-list
  :rule 3bmd-grammar::doc
  :text "* First line

  ***

  Second line
"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "First" " " "line")))
              ;; TODO: This horizontal rule and following
              ;; text should be a part of the previous LIST-ITEM
              (:HORIZONTAL-RULE)
              (:PLAIN "  " "Second" " " "line")))
