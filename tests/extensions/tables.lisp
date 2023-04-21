(fiasco:define-test-package #:3bmd-table-tests
  (:import-from #:3bmd-grammar
                #:th
                #:td
                #:left
                #:center
                #:right))

(in-package #:3bmd-table-tests)


(3bmd-tests::def-grammar-test tables-1
  :enable-extensions 3bmd-tables:*tables*
  :rule 3bmd-grammar::table
  :text "| First Header  | Second Header |
| ------------- | ------------- |
| Content Cell  | Content cell  |
| content Cell  | content cell  |
"
  :expected '(3bmd::table :head
              (((th (:plain "First" " " "Header") nil)
                (th (:plain "Second" " " "Header") nil)))
              :body
              (((td (:plain "Content" " " "Cell") nil)
                (td (:plain "Content" " " "cell") nil))
               ((td (:plain "content" " " "Cell") nil)
                (td (:plain "content" " " "cell") nil)))))

(3bmd-tests::def-grammar-test tables-2
  :enable-extensions 3bmd-tables:*tables*
  :rule 3bmd-grammar::table
  :text "| Name | Description          |
| ------------- | ----------- |
| Help      | Display the help window.|
| Close     | Closes a window     |
"
  :expected ' (3BMD::TABLE :HEAD
                           (((TH (:PLAIN "Name") NIL)
                             (TH (:PLAIN "Description") NIL)))
                           :BODY
                           (((TD (:PLAIN "Help") NIL)
                             (TD (:PLAIN "Display" " " "the" " "
                                         "help" " " "window.")
                                 NIL))
                            ((TD (:PLAIN "Close") NIL)
                             (TD (:PLAIN "Closes" " " "a" " " "window") NIL)))))


(3bmd-tests::def-grammar-test tables-3
  :enable-extensions 3bmd-tables:*tables*
  :rule 3bmd-grammar::table
  :text "| Left-Aligned  | Center Aligned  | Right Aligned |
| :------------ |:---------------:| -----:|
| col `3` is    | some wordy text | $1600 |
| col 2 is      | centered        |   $12 |
| zebra stripes | are neat        |    $1 |
"
  :expected '(3BMD::TABLE :HEAD
                          (((TH (:PLAIN "Left-Aligned") LEFT)
                            (TH (:PLAIN "Center" " " "Aligned") CENTER)
                            (TH (:PLAIN "Right" " " "Aligned") RIGHT)))
                          :BODY
                          (((TD (:PLAIN "col" " " (:CODE "3") " " "is") LEFT)
                            (TD (:PLAIN "some" " " "wordy" " " "text") CENTER)
                            (TD (:PLAIN "$1600") RIGHT))
                           ((TD (:PLAIN "col" " " "2" " " "is") LEFT)
                            (TD (:PLAIN "centered") CENTER)
                            (TD (:PLAIN "$12") RIGHT))
                           ((TD (:PLAIN "zebra" " " "stripes") LEFT)
                            (TD (:PLAIN "are" " " "neat") CENTER)
                            (TD (:PLAIN "$1") RIGHT)))))


(3bmd-tests::def-grammar-test tables-4
  :enable-extensions 3bmd-tables:*tables*
  :rule 3bmd-grammar::table
  :text "| a | b |  
|---|---|   
|    1 a b`|`| 2 |  
| 3 \\|| 4 |    
| > 5 | ### 6 |
"
  :expected '(3BMD::TABLE
              :HEAD
              (((TH (:PLAIN "a") NIL)
                (TH (:PLAIN "b") NIL)))
              :BODY
              (((TD (:PLAIN "1" " " "a" " " "b" (:CODE "|")) NIL)
                (TD (:PLAIN "2") NIL))
               ((TD (:PLAIN "3" " " "|") NIL)
                (TD (:PLAIN "4") NIL))
               ((TD (:PLAIN ">" " " "5") NIL)
                (TD (:PLAIN "#" "#" "#" " " "6") NIL)))))

(3bmd-tests::def-grammar-test tables-5
  :enable-extensions 3bmd-tables:*tables*
  :rule 3bmd-grammar::table
  :text "| a |   |  
| - | - |   
|   | b |
"
  :expected '(3BMD::TABLE
              :HEAD
              (((TH (:PLAIN "a") NIL)
                (TH (:PLAIN) NIL)))
              :BODY
              (((TD (:PLAIN) NIL)
                (TD (:PLAIN "b") NIL)))))

(3bmd-tests::def-grammar-test tables-6
  :enable-extensions 3bmd-tables:*tables*
  :rule 3bmd-grammar::table
  :text  "|\\|||  
| - | - |   
||.|
"
  :expected '(3BMD::TABLE
              :HEAD
              (((TH (:PLAIN "|") NIL)
                (TH (:PLAIN) NIL)))
              :BODY
              (((TD (:PLAIN) NIL)
                (TD (:PLAIN ".") NIL)))))
