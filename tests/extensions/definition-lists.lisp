(fiasco:define-test-package #:3bmd-definition-list-tests
  (:import-from #:3bmd-definition-lists
                #:definition-list
                #:definition-list-item
                #:definition-term))

(in-package #:3bmd-definition-list-tests)

(3bmd-tests::def-grammar-test definition-list-1
  :enable-extensions 3bmd-definition-lists:*definition-lists*
  :rule 3bmd-grammar::%block
  :text "term1 `q`
term2
:  def
  def continued
"
  :expected '(DEFINITION-LIST
              (:TERMS
               ((DEFINITION-TERM "term1" " " (:CODE "q"))
                (DEFINITION-TERM "term2"))
               :DEFINITIONS
               ((DEFINITION-LIST-ITEM (:PLAIN "def" "
"
                                       "  " "def" " " "continued"))))))

(3bmd-tests::def-grammar-test definition-list-2
  :enable-extensions 3bmd-definition-lists:*definition-lists*
  :rule 3bmd-grammar::%block
  :text "term1
:   definition 1
    def 1 cont

term 2
:   definition 2

"
  :expected '(DEFINITION-LIST
              (:TERMS
               ((DEFINITION-TERM "term1"))
               :DEFINITIONS
               ((DEFINITION-LIST-ITEM (:PLAIN "definition" " " "1" "
"
                                       "def" " " "1" " " "cont"))))
              (:TERMS
               ((DEFINITION-TERM "term" " " "2"))
               :DEFINITIONS
               ((DEFINITION-LIST-ITEM (:PLAIN "definition" " " "2"))))))

;; todo: add remaining tests from extension
