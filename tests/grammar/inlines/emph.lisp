(in-package 3bmd-tests)


(def-grammar-test emph-test-1
  :rule 3bmd-grammar::emph
  :text "*Some other text*"
  :expected '(:EMPH "Some" " " "other" " " "text"))

(def-grammar-test emph-test-2
  :rule 3bmd-grammar::emph
  :text "_Some other text_"
  :expected '(:EMPH "Some" " " "other" " " "text"))

(def-grammar-test emph-test-3
  :rule 3bmd-grammar::emph
  :text "_*Some other text*_"
  ;; Two nested emph are allowed but not make any sence.
  ;; CommonMark's reference parser generates them as well.
  :expected '(:EMPH
              (:EMPH "Some" " " "other" " " "text")))

(def-grammar-test emph-test-4
  :rule 3bmd-grammar::emph
  :text "*Some other text**"
  :expected '(:EMPH "Some" " " "other" " " "text")
  :remaining-text "*")

(def-grammar-test emph-test-5
  :rule 3bmd-grammar::emph
  :text "*Some other text *"
  :fail-expected 'esrap:esrap-parse-error)


(def-grammar-test emph-nested-in-a-list-test-1
  :rule 3bmd-grammar::doc
  :text "* *Some text*"
  :expected '((:BULLET-LIST
               (:LIST-ITEM
                (:PLAIN (:EMPH "Some" " " "text"))))))


(def-grammar-test emph-nested-in-a-list-test-2
  :rule 3bmd-grammar::doc
  :text "* First line
  *Some text*"
  :expected '((:BULLET-LIST
               (:LIST-ITEM
                (:PLAIN
                 "First"
                 " "
                 "line"
                 "
"
                 ;; TODO: We should not have these indentation
                 ;; spaces before the "Some text", because
                 ;; list item's body is indented after the bullet item.
                 "  "
                 (:EMPH "Some" " " "text"))))))

(def-grammar-test emph-nested-in-a-list-test-3
  :rule 3bmd-grammar::doc
  :text "* First line
 *Some text*"
  :expected '((:BULLET-LIST
               (:LIST-ITEM
                (:PLAIN
                 "First"
                 " "
                 "line"
                 "
"
                 ;; TODO: We should not have these indentation
                 ;; spaces before the "Some text", because
                 ;; list item's body is indented
                 " "
                 (:EMPH "Some" " " "text"))))))

(def-grammar-test emph-nested-in-a-list-test-4
  :rule 3bmd-grammar::doc
  :text "* First line
*Some text*"
  :expected '((:BULLET-LIST
               (:LIST-ITEM
                (:PLAIN
                 "First"
                 " "
                 "line"
                 "
"
                 (:EMPH "Some" " " "text"))))))

(def-grammar-test emph-nested-in-a-list-test-5
  :rule 3bmd-grammar::doc
  :text "* First line

  *Some text*

"
  ;; :really-expected '((:BULLET-LIST
  ;;                     (:LIST-ITEM
  ;;                      (:PARAGRAPH "First" " " "line")
  ;;                      (:PARAGRAPH (:EMPH "Some" " " "text")))))
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "First" " " "line")))
              (:PARAGRAPH (:EMPH "Some" " " "text"))))
