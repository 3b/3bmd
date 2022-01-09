(in-package 3bmd-tests)

;; TODO:
;; For some reason 3BMD does not conside a code block as a block rule.
;; That is why these tests are placed in inlines.
;;
;; CommonMark separates inline code spans:
;; https://spec.commonmark.org/0.30/#code-spans
;; from indented and fenced code blocks:
;; https://spec.commonmark.org/0.30/#fenced-code-blocks

(def-grammar-test code-test-1
  :rule 3bmd-grammar::code
  :text "`Some code`"
  :expected '(:CODE "Some code"))

(def-grammar-test code-test-2
  :rule 3bmd-grammar::code
  :text "``Some code``"
  :expected '(:CODE "Some code"))

(def-grammar-test code-test-3
  :rule 3bmd-grammar::code
  :text "```Some code```"
  :expected '(:CODE "Some code"))


;; In the next two tests we can
;; see, 3BMD makes no difference between
;; fenced and inline code blocks,
;; whereas CommonMark renders fenced
;; blocks as <pre><code>... and
;; inline code as a single <code> elements.

(def-grammar-test code-test-4
  :rule 3bmd-grammar::code
  :text "```
Some
code
```"
  :expected '(:CODE "
Some
code
"))

(def-grammar-test code-test-5
  :rule 3bmd-grammar::code
  :text "`
Some
code
`"
  :expected '(:CODE "
Some
code
"))


;; Also, 3BMD ignores a programming language
(def-grammar-test code-test-6
  :rule 3bmd-grammar::code
  :text "```python
Some
code
```"
  :expected '(:CODE "python
Some
code
"))

;; However, you might enable an extension
;; which will parse fenced code-blocks propertly
(def-grammar-test code-test-7
  :enable-extensions 3bmd-code-blocks:*code-blocks*
  :rule 3bmd-code-blocks::code-block
  :text "```python
Some
code
```"
  :expected '(3BMD-CODE-BLOCKS::CODE-BLOCK
              :LANG "python"
              :PARAMS NIL
              :CONTENT "Some
code"))


;; Now we check how does it work with nesting in a list

(def-grammar-test code-test-8
  :rule 3bmd-grammar::doc
  :text "* A list item

  ```
  Some
  code
  ```"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "A" " " "list" " " "item")))
              ;; TODO:
              ;; This should be parsed as a part of the LIST-ITEM
              ;; and without any indentation
              (:PLAIN "  "
               (:CODE "
  Some
  code
"))))


(def-grammar-test code-test-9
  :enable-extensions 3bmd-code-blocks:*code-blocks*
  :rule 3bmd-grammar::doc
  :text "* A list item

  ```python
  Some
  code
  ```"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "A" " " "list" " " "item")))
              ;; TODO:
              ;; Again, this should be a part of the LIST-ITEM.
              ;; Also, extension didn't recognize the block et all.
              ;; and this should be fixed too.
              (:PLAIN "  "
               (:CODE "python
  Some
  code
"))))
