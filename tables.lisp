;;; PHP Markdown Extra style tables

;; Only these table styles are supported, because they are unambigous to parse:
;;
;; | First Header  | Second Header |
;; | ------------- | ------------- |
;; | Content Cell  | Content Cell  |
;; | Content Cell  | Content Cell  |
;;
;; Note that the dashes at the top don't need to match the length of the header text exactly:
;;
;; | Name | Description          |
;; | ------------- | ----------- |
;; | Help      | Display the help window.|
;; | Close     | Closes a window     |
;;
;; By including colons : within the header row, you can define text to be
;; left-aligned, right-aligned, or center-aligned:
;;
;; | Left-Aligned  | Center Aligned  | Right Aligned |
;; | :------------ |:---------------:| -----:|
;; | col 3 is      | some wordy text | $1600 |
;; | col 2 is      | centered        |   $12 |
;; | zebra stripes | are neat        |    $1 |

(defpackage #:3bmd-tables
  (:export #:*tables*
           #:*table-class*))

(in-package #:3bmd-grammar)

(defparameter 3bmd-tables:*table-class* nil
  "Table class to be used in the rendered HTML")

(defrule table-cell (and #\|
                         sp
                         (* (and (! (or (and sp #\|) endline)) inline))
                         sp
                         (& #\|))
  (:destructure (_ __ content &rest ___)
    (declare (ignore _ __ ___))
    (mapcar 'second content)))

(defrule table-row (and (& #\|) (+ table-cell) #\| sp newline)
  (:destructure (_ cells &rest __)
    (declare (ignore _ __))
    (mapcar (lambda (a) (cons :plain a)) cells)))


(defrule table-align-cell (and sp (? #\:) (+ #\-) (? #\:) sp #\|)
  (:destructure (_ left __ right &rest ___)
    (declare (ignore _ __ ___))
    (if right (if left 'center 'right) (when left 'left))))

(defrule table-align-row (and #\| (+ table-align-cell) sp newline)
  (:destructure (_ aligns &rest __)
    (declare (ignore _ __))
    aligns))

(defrule table-head (and table-row table-align-row))

(define-extension-block 3bmd-tables:*tables* table
    (and (? nonindent-space) (? table-head) (+ table-row))
  (:destructure (_ (&optional header aligns) rows)
    (declare (ignore _))
    (let* ((max-length (reduce 'max (mapcar 'length (cons header rows))))
           (aligns (append aligns
                           (make-list (max 0 (- max-length (length aligns)))
                                      :initial-element nil))))
      (list '3bmd::table
            :head (list (mapcar (lambda (content align)
                             (list 'th content align))
                           header aligns))
            :body (mapcar (lambda (row)
                            (mapcar (lambda (content align)
                                      (list 'td content align))
                                    row aligns))
                          rows))))
  (:escape-char-rule table-escaped-characters #\|)
  (:character-rule table-extended-characters #\|))

(in-package #:3bmd)

(defmethod print-tagged-element ((tag (eql 'table)) stream rest)
  (padded (1 stream)
    (format stream "<table ~@[class=\"~a\"~]>" 3bmd-tables:*table-class*))
  (when (getf rest :head)
    (padded (1 stream)
      (format stream "<thead>"))
    (loop for row in (getf rest :head)
          do (print-tagged-element 'tr stream row))
    (padded (1 stream)
      (format stream "</thead>")))
  (padded (1 stream)
    (format stream "<tbody>"))
  (loop for row in (getf rest :body)
        do (print-tagged-element 'tr stream row))
  (padded (1 stream)
    (format stream "</tbody>"))
  (padded (1 stream)
    (format stream "</table>")))

(defmethod print-tagged-element ((tag (eql 'tr)) stream rest)
  (padded (1 stream)
    (format stream "<tr>"))
  (dolist (cell rest)
    (format stream "<~(~A~)~A>"
            (first cell)
            (if (third cell)
                (format nil " align=\"~(~A~)\"" (third cell))
                ""))
    (print-element (second cell) stream)
    (format stream "</~(~A~)>" (first cell)))
  (format stream "</tr>"))

;;; tests

#+should-test (progn

(use-package :should-test)
(shadowing-import '(3bmd-grammar::th 3bmd-grammar::td
                    3bmd-grammar::left 3bmd-grammar::center
                    3bmd-grammar::right))
(deftest parse-table ()
  (should be equal '((table
                      :head
                      (((th (:plain "First" " " "Header") nil)
                        (th (:plain "Second" " " "Header") nil)))
                      :body
                      (((td (:plain "Content" " " "Cell") nil)
                        (td (:plain "Content" " " "Cell") nil))
                       ((td (:plain "Content" " " "Cell") nil)
                        (td (:plain "Content" " " "Cell") nil)))))
          (parse-doc "
| First Header  | Second Header |
| ------------- | ------------- |
| Content Cell  | Content Cell  |
| Content Cell  | Content Cell  |
"))
  (should be equal '((table
                      :head
                      (((th (:plain "Name") nil)
                        (th (:plain "Description") nil)))
                      :body
                      (((td (:plain "Help") nil)
                        (td (:plain "Display" " " "the" " " "help" " " "window.") nil))
                       ((td (:plain "Close") nil)
                        (td (:plain "Closes" " " "a" " " "window") nil)))))
          (parse-doc "
| Name | Description          |
| ------------- | ----------- |
| Help      | Display the help window.|
| Close     | Closes a window     |
"))  (should be equal '((table
                         :head
                         (((th (:plain "Left-Aligned") left)
                           (th (:plain "Center" " " "Aligned") center)
                           (th (:plain "Right" " " "Aligned") right)))
                         :body
                         (((td (:plain "col" " " (:code "3") " " "is") left)
                           (td (:plain "some" " " "wordy" " " "text") center)
                           (td (:plain "$1600") right))
                          ((td (:plain "col" " " "2" " " "is") left)
                           (td (:plain "centered") center)
                           (td (:plain "$12") right))
                          ((td (:plain "zebra" " " "stripes") left)
                           (td (:plain "are" " " "neat") center)
                           (td (:plain "$1") right)))))
          (parse-doc "
| Left-Aligned  | Center Aligned  | Right Aligned |
| :------------ |:---------------:| -----:|
| col `3` is    | some wordy text | $1600 |
| col 2 is      | centered        |   $12 |
| zebra stripes | are neat        |    $1 |
"))

 (should be equal '((TABLE
                     :head
                     (((TH (:PLAIN "a") NIL)
                       (TH (:PLAIN "b") NIL)))
                     :body
                     (((TD (:PLAIN "1" " " "a" " " "b" (:CODE "|")) NIL)
                       (TD (:PLAIN "2") NIL))
                      ((TD (:PLAIN "3" " " "|") NIL)
                       (TD (:PLAIN "4") NIL))
                      ((TD (:PLAIN ">" " " "5") NIL)
                       (TD (:PLAIN "#" "#" "#" " " "6") NIL)))))
         (parse-doc "
| a | b |  
|---|---|   
|    1 a b`|`| 2 |  
| 3 \\|| 4 |    
| > 5 | ### 6 |
"))
 (should be equal '((TABLE
                     :HEAD
                     (((3BMD-GRAMMAR::TH (:PLAIN "a") NIL)
                       (3BMD-GRAMMAR::TH (:PLAIN) NIL)))
                     :BODY
                     (((3BMD-GRAMMAR::TD (:PLAIN) NIL)
                       (3BMD-GRAMMAR::TD (:PLAIN "b") NIL)))))
         (parse-doc "
| a |   |  
| - | - |   
|   | b |
"))
 (should be equal '((TABLE
                     :HEAD
                     (((3BMD-GRAMMAR::TH (:PLAIN "|") NIL)
                       (3BMD-GRAMMAR::TH (:PLAIN) NIL)))
                     :BODY
                     (((3BMD-GRAMMAR::TD (:PLAIN) NIL)
                       (3BMD-GRAMMAR::TD (:PLAIN ".") NIL)))))
         (parse-doc "
|\\|||  
| - | - |   
||.|
"))
)
)
