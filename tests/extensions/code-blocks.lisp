(fiasco:define-test-package #:3bmd-code-block-tests
  (:use #:3bmd-tests)
  (:import-from #:3bmd-code-blocks
))

(in-package #:3bmd-code-block-tests)

(3bmd-tests::def-grammar-test code-block-1
  :enable-extensions 3bmd-code-blocks:*code-blocks*
  :rule 3bmd-grammar::%block
  :text "```language
code \"*_`[]{}```
 *1*
>  2
   3
    4
     5
```
"
  :expected '(3BMD-CODE-BLOCKS::CODE-BLOCK :LANG
                          "language" :PARAMS NIL :CONTENT "code \"*_`[]{}```
 *1*
>  2
   3
    4
     5"))

(3bmd-tests::def-grammar-test code-block-indented-1
  :enable-extensions 3bmd-code-blocks:*code-blocks*
  :rule 3bmd-grammar::%block
  :text "
- xxx

    ```language
    code \"*_`[]{}```
     1
      2
       3
        4
         5
    ```

"
  :expected '(:BULLET-LIST
                          (:LIST-ITEM (:PARAGRAPH "xxx")
                           (3BMD-CODE-BLOCKS::CODE-BLOCK :LANG
                            "language" :PARAMS NIL :CONTENT
                            "code \"*_`[]{}```
 1
  2
   3
    4
     5"))))


(def-print-test print-code-block-indented-1 ;; bug #57
  :enable-extensions 3bmd-code-blocks:*code-blocks*
  :format :markdown
  :text "
- xxx

    ```language
    code \"*_`[]{}```
     1
      2
       3
        4
         5
    ```
"
  :expected "- xxx

    ```language
    code \"*_`[]{}```
     1
      2
       3
        4
         5
    ```
")

(def-print-test print-code-block-indented-2
  :enable-extensions 3bmd-code-blocks:*code-blocks*
  :format :markdown
  :text "
- xxx

    - yyy
        > ```language
        > code \"*_`[]{}```
        > > 1
        > >  2
        >    3
        >     4
        >      5
        > ```
"
  :expected "- xxx

    - yyy

        > ```language
        > code \"*_`[]{}```
        > > 1
        > >  2
        >    3
        >     4
        >      5
        > ```
")
