(in-package 3bmd-tests)


(deftest parse-simple-list ()
  (let ((expected '((:BULLET-LIST
                     (:LIST-ITEM (:PLAIN "Foo"))
                     (:LIST-ITEM (:PLAIN "Bar"))))))
    (multiple-value-bind (result remaining-text parse-succeeded)
        (3bmd-grammar:parse-doc ' "* Foo
* Bar
")
      (is (equalp result expected))
      (is (not remaining-text))
      (is parse-succeeded))))


(deftest parse-list-with-separated-items ()
  (let ((expected '((:BULLET-LIST
                     (:LIST-ITEM (:PARAGRAPH "Foo"))
                     (:LIST-ITEM (:PARAGRAPH "Bar"))))))
    (multiple-value-bind (result remaining-text parse-succeeded)
        (3bmd-grammar:parse-doc "* Foo

* Bar
")
      (is (equalp result expected))
      (is (not remaining-text))
      (is parse-succeeded))))


(deftest parse-list-with-multiline-item ()
  (let ((expected '((:BULLET-LIST
                     (:LIST-ITEM
                      ;; TODO:
                      ;; Seems in this case this item should be a PARAGRAPH too?
                      (:PLAIN "Foo")))
                    ;; TODO:
                    ;; Why this paragraph is not part of the previos list item?
                    ;; It has the same indentation as the list item.
                    (:PARAGRAPH "Second" " " "line")
                    ;; TODO:
                    ;; Why do this "Bar is not a second LIST-ITEM of the first BULLET-LIST?
                    (:BULLET-LIST
                     (:LIST-ITEM (:PLAIN "Bar"))))))
    (multiple-value-bind (result remaining-text parse-succeeded)
        (3bmd-grammar:parse-doc "* Foo

  Second line

* Bar
")
      (is (equalp result expected))
      (is (not remaining-text))
      (is parse-succeeded))))


(deftest parse-list-with-embedded-code ()
  (let ((expected '((:BULLET-LIST (:LIST-ITEM (:PLAIN "Foo")))
                    ;; TODO:
                    ;; This paragraph should be a part of the first list item
                    (:PARAGRAPH
                     (:CODE "
  This is a code
"))
                    ;; And this bullet list should not be generated.
                    ;; The Bar list item should be inside the first bullet list.
                    (:BULLET-LIST (:LIST-ITEM (:PLAIN "Bar")))))
    ;;     (real-expected '((:BULLET-LIST
    ;;                       (:LIST-ITEM
    ;;                        (:PARAGRAPH "Foo")
    ;;                        (:PARAGRAPH (:CODE "
    ;;   This is a code
    ;; ")))
    ;;                  (:LIST-ITEM (:PLAIN "Bar")))))
        )
    (multiple-value-bind (result remaining-text parse-succeeded)
        (3bmd-grammar:parse-doc "* Foo

  ```
  This is a code
  ```

* Bar
")
      (is (equalp result expected))
      (is (not remaining-text))
      (is parse-succeeded))))
