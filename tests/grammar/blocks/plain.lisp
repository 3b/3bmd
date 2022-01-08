(in-package 3bmd-tests)


;; Plain text is similar to paragraph
(def-grammar-test parse-plain-text
  :text "Foo bar.
Second line.
"
  :expected '((:PLAIN
               "Foo"
               " "
               "bar."
               "
"
               "Second"
               " "
               "line.")))


(def-grammar-test parse-plain-text-with-inlines
  :text "Foo `bar` ![](http://some.com/image)
Second *line*."
  :expected '((:PLAIN
               "Foo"
               " "
               (:CODE "bar")
               " "
               (:IMAGE
                (:EXPLICIT-LINK :LABEL NIL :SOURCE "http://some.com/image" :TITLE NIL))
               "
"
               "Second"
               " "
               (:EMPH "line")
               ".")))
