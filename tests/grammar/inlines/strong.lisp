(in-package 3bmd-tests)


(def-grammar-test strong-test-1
  :rule 3bmd-grammar::strong
  :text "**Some other text**"
  :expected '(:STRONG "Some" " " "other" " " "text"))

(def-grammar-test strong-test-2
  :rule 3bmd-grammar::strong
  :text "__Some other text__"
  :expected '(:STRONG "Some" " " "other" " " "text"))

(def-grammar-test strong-test-3
  :rule 3bmd-grammar::strong
  :text "***Some other text***"
  ;; This almost matches to CommonMark spec, but
  ;; it's reference implementation from
  ;; https://spec.commonmark.org/dingus/
  ;; makes EM outer element.
  :expected '(:STRONG
              (:EMPH "Some" " " "other" " " "text")))

(def-grammar-test strong-test-4
  :rule 3bmd-grammar::strong
  :text "___Some other text___"
  ;; The same as with stars:
  :expected '(:STRONG
              (:EMPH "Some" " " "other" " " "text")))

(def-grammar-test strong-test-5
  :rule 3bmd-grammar::strong
  :text "**Some other text***"
  :expected '(:STRONG "Some" " " "other" " " "text")
  :remaining-text "*")

(def-grammar-test strong-test-6
  :rule 3bmd-grammar::strong
  :text "**Some other text **"
  :fail-expected 'esrap:esrap-parse-error)


(def-grammar-test strong-nested-in-a-list-test-1
  :rule 3bmd-grammar::doc
  :text "* **Some text**"
  :expected '((:BULLET-LIST
               (:LIST-ITEM
                (:PLAIN (:STRONG "Some" " " "text"))))))


(def-grammar-test strong-nested-in-a-list-test-2
  :rule 3bmd-grammar::doc
  :text "* First line
  **Some text**"
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
                 (:STRONG "Some" " " "text"))))))

(def-grammar-test strong-nested-in-a-list-test-3
  :rule 3bmd-grammar::doc
  :text "* First line
 **Some text**"
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
                 (:STRONG "Some" " " "text"))))))

(def-grammar-test strong-nested-in-a-list-test-4
  :rule 3bmd-grammar::doc
  :text "* First line
**Some text**"
  :expected '((:BULLET-LIST
               (:LIST-ITEM
                (:PLAIN
                 "First"
                 " "
                 "line"
                 "
"
                 (:STRONG "Some" " " "text"))))))

(def-grammar-test strong-nested-in-a-list-test-5
  :rule 3bmd-grammar::doc
  :text "* First line

  **Some text**

"
  ;; :really-expected '((:BULLET-LIST
  ;;                     (:LIST-ITEM
  ;;                      (:PARAGRAPH "First" " " "line")
  ;;                      (:PARAGRAPH (:STRONG "Some" " " "text")))))
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "First" " " "line")))
              (:PARAGRAPH (:STRONG "Some" " " "text"))))
