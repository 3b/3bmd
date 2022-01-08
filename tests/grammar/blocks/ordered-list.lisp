(in-package 3bmd-tests)


(def-grammar-test parse-simple-ordered-list-1
  :rule 3bmd-grammar::ordered-list
  :text "1. Foo
2. Bar
"
  :expected '(:COUNTED-LIST
              (:LIST-ITEM (:PLAIN "Foo"))
              (:LIST-ITEM (:PLAIN "Bar"))))

(def-grammar-test parse-simple-ordered-list-2
  :rule 3bmd-grammar::ordered-list
  :text "1) Foo
2) Bar
"
  ;; TODO: This type of list markers are not
  ;; described in the original markdown format
  ;; https://daringfireball.net/projects/markdown/syntax#list
  ;; but is in the CommonMark spec.
  :fail-expected 'esrap:esrap-parse-error
  ;; :expected '(:COUNTED-LIST
  ;;             (:LIST-ITEM (:PLAIN "Foo"))
  ;;             (:LIST-ITEM (:PLAIN "Bar")))
  )

(def-grammar-test parse-simple-ordered-list-3
  :rule 3bmd-grammar::ordered-list
  :text "7. Foo
8. Bar
"
  ;; TODO: In Common Mark list can start from a number
  ;; other than 1. In this case a special attribute START
  ;; is used to remember the start number
  :expected '(:COUNTED-LIST
              (:LIST-ITEM (:PLAIN "Foo"))
              (:LIST-ITEM (:PLAIN "Bar"))))


(def-grammar-test parse-ordered-list-with-separated-items
  :rule 3bmd-grammar::ordered-list
  :text "1. Foo

2. Bar
"
  :expected '(:COUNTED-LIST
              (:LIST-ITEM (:PARAGRAPH "Foo"))
              (:LIST-ITEM (:PARAGRAPH "Bar"))))


(def-grammar-test parse-ordered-list-with-multiline-item
  :text "1. Foo

   Second line

2. Bar
"
  ;; TODO: This should work as well. But right now generates
  ;; two separate lists and a paragraph between them:
  :expected '((:COUNTED-LIST (:LIST-ITEM (:PLAIN "Foo")))
              (:PARAGRAPH "Second" " " "line")
              (:COUNTED-LIST (:LIST-ITEM (:PLAIN "Bar")))))


(def-grammar-test parse-ordered-list-with-embedded-code
  :text "1. Foo

  ```
  This is a code
  ```

2. Bar
"
;;   :really-expected '((:COUNTED-LIST
;;                       (:LIST-ITEM
;;                        (:PLAIN "Foo"))
;;                       (:LIST-ITEM
;;                        (:PARAGRAPH
;;                         (:CODE "
;;   This is a code
;; ")))
;;                       (:LIST-ITEM
;;                        (:PLAIN "Bar"))))
  :expected '((:COUNTED-LIST
               (:LIST-ITEM (:PLAIN "Foo")))
              (:PARAGRAPH
               (:CODE "
  This is a code
"))
              (:COUNTED-LIST
               (:LIST-ITEM (:PLAIN "Bar")))))
