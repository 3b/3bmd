(in-package 3bmd-tests)


(def-grammar-test ellipsis1
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::smart
  :text "..."
  :expected '(:ellipsis "..."))

(def-grammar-test ellipsis2
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::smart
  :text ". . ."
  :expected '(:ellipsis ". . ."))

(def-grammar-test dash1
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::smart
  :text "--"
  :expected '(:en-dash "--"))

(def-grammar-test dash2
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::smart
  :text "---"
  :expected '(:em-dash "---"))


(def-grammar-test single-quoted1
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::smart
  :text "'text'"
  :expected '(:single-quoted "text"))

(def-grammar-test single-quoted2
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::smart
  ;; heuristic to detect contractions
  :text "'tex't'"
  :expected '(:single-quoted "tex" :apostrophe "t"))

(def-grammar-test single-quoted3
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::smart
  :text "'te'xt' a'"
  :expected '(:single-quoted "te"))

(def-grammar-test single-quoted3
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::%block
  :text "'te'xt' a'"
  ;; todo: check common mark spec to see how these should parse
  :fail-expected t
  :expected '(:plain (:single-quoted "te" :apostrophe "xt" :apostrophe " a")))


(def-grammar-test single-quoted4
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::%block
  :text "'te'xt'a'"
  :fail-expected t ;; todo: this should parse
  :expected '(:plain (:single-quoted "te" :apostrophe "xt" :apostrophe "a")))

(def-grammar-test single-quoted5
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::%block
  :text "'te'xt 'a'"
  :fail-expected t  ;; todo: this should parse (not sure exact results though?
  :expected '(:plain (:single-quoted "te") "xt" " " (:single-quoted "a")))

(def-grammar-test double-quote-1
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::%block
  :text "\"text\""
  :expected '(:PLAIN (:DOUBLE-QUOTED "text")))

(def-grammar-test arrows
  :enable-extensions 3bmd-grammar::*smart-quotes*
  :rule 3bmd-grammar::%block
  :text "<-> <- -> <=> <= => =<"
  :expected '(:plain
              (:left-right-single-arrow "<->") " "
              (:left-single-arrow "<-") " "
              (:right-single-arrow "->") " "
              (:left-right-double-arrow "<=>") " "
              (:left-double-arrow "<=") " "
              (:right-double-arrow "=>") " "
              "=" "<"))
