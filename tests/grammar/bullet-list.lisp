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
                      (:PARAGRAPH "Foo")
                      (:PARAGRAPH "Second" "line"))
                     (:LIST-ITEM (:PARAGRAPH "Bar"))))))
    (multiple-value-bind (result remaining-text parse-succeeded)
        (3bmd-grammar:parse-doc "* Foo

    Second line

* Bar
")
      (is (equalp result expected))
      (is (not remaining-text))
      (is parse-succeeded))))


(deftest parse-list-with-embedded-code ()
  (let ((expected '((:BULLET-LIST
                     (:LIST-ITEM
                      (:PARAGRAPH "Foo")
                      (:PARAGRAPH (:CODE "
      This is a code
    ")))
                     (:LIST-ITEM (:PLAIN "Bar"))))))
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
