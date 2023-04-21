(fiasco:define-test-package #:3bmd-definition-list-tests
  (:use #:3bmd-tests)
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


(def-print-test print-definition-lists-1 ;; bug #43
  :enable-extensions 3bmd-definition-lists:*definition-lists*
  :format :markdown
  :text "term1 `q`
term2
:  def
  def continued
"
  :expected "term1 `q`
term2
: def
  def continued
")

(def-print-test print-definition-lists-2 ;; bug #43
  :enable-extensions 3bmd-definition-lists:*definition-lists*
  :text "term1 `q`
term2
:  def
  def continued
"
  :expected "<dl>

<dt>term1 <code>q</code></dt>
<dt>term2</dt>
<dd>def
  def continued</dd>
</dl>
")

;; todo: add remaining tests from extension
