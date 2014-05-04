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
  (:export #:*tables*))


(in-package #:3bmd-grammar)


(defrule table-cell (and #\|
                         sp
                         (+ (and (! (or (and sp #\|) endline)) inline))
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
      (cons '3bmd::table
            (let ((hs (mapcar (lambda (content align)
                                (list 'th content align))
                              header aligns))
                  (rs (mapcar (lambda (row)
                                (mapcar (lambda (content align)
                                          (list 'td content align))
                                        row aligns))
                              rows)))
              (if hs (cons hs rs) rs))))))

(in-package #:3bmd)

(defmethod print-tagged-element ((tag (eql 'table)) stream rest)
  (padded (1 stream)
    (format stream "<table>"))
  (loop :for row :in rest :do
     (print-tagged-element 'tr stream row))
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

(deftest parse-table ()
  (should be equal '((table
                      ((th (:plain "First" " " "Header") nil)
                       (th (:plain "Second" " " "Header") nil))
                      ((td (:plain "Content" " " "Cell") nil)
                       (td (:plain "Content" " " "Cell") nil))
                      ((td (:plain "Content" " " "Cell") nil)
                       (td (:plain "Content" " " "Cell") nil))))
          (parse-doc "
| First Header  | Second Header |
| ------------- | ------------- |
| Content Cell  | Content Cell  |
| Content Cell  | Content Cell  |
"))
  (should be equal '((table
                      ((th (:plain "Name") nil)
                       (th (:plain "Description") nil))
                      ((td (:plain "Help") nil)
                       (td (:plain "Display" " " "the" " " "help" " " "window.") nil))
                      ((td (:plain "Close") nil)
                       (td (:plain "Closes" " " "a" " " "window") nil))))
          (parse-doc "
| Name | Description          |
| ------------- | ----------- |
| Help      | Display the help window.|
| Close     | Closes a window     |
"))  (should be equal '((table
                         ((th (:plain "Left-Aligned") left)
                          (th (:plain "Center" " " "Aligned") center)
                          (th (:plain "Right" " " "Aligned") right))
                         ((td (:plain "col" " " (:code "3") " " "is") left)
                          (td (:plain "some" " " "wordy" " " "text") center)
                          (td (:plain "$1600") right))
                         ((td (:plain "col" " " "2" " " "is") left)
                          (td (:plain "centered") center)
                          (td (:plain "$12") right))
                         ((td (:plain "zebra" " " "stripes") left)
                          (td (:plain "are" " " "neat") center)
                          (td (:plain "$1") right))))
          (parse-doc "
| Left-Aligned  | Center Aligned  | Right Aligned |
| :------------ |:---------------:| -----:|
| col `3` is    | some wordy text | $1600 |
| col 2 is      | centered        |   $12 |
| zebra stripes | are neat        |    $1 |
"))

 (should be equal '((TABLE
                     ((TH (:PLAIN "a") NIL)
                      (TH (:PLAIN "b") NIL))
                     ((TD (:PLAIN "1" " " "a" " " "b" (:CODE "|")) NIL)
                      (TD (:PLAIN "2") NIL))
                     ((TD (:PLAIN "3" " " "|") NIL)
                      (TD (:PLAIN "4") NIL))
                     ((TD (:PLAIN ">" " " "5") NIL)
                      (TD (:PLAIN "#" "#" "#" " " "6") NIL))))
         (parse-doc "
| a | b |  
|---|---|   
|    1 a b`|`| 2 |  
| 3 \\|| 4 |    
| > 5 | ### 6 |
"))
)
)
