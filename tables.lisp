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

(defun table-cell-char-p (char)
  (not (member char '(#\Newline #\Return #\Linefeed #\|))))

(defrule table-cell-character (table-cell-char-p character))

(defrule table-cell (and sp (+ table-cell-character) sp #\|)
  (:destructure (_ content &rest __)
    (declare (ignore _ __))
    (string-right-trim '(#\space #\tab) (coerce content 'string))))

(defrule table-row (and #\| (+ table-cell) newline)
  (:destructure (_ cells __)
    (declare (ignore _ __))
    cells))

(defrule table-align-cell (and sp (? #\:) (+ #\-) (? #\:) sp #\|)
  (:destructure (_ left __ right &rest ___)
    (declare (ignore _ __ ___))
    (if right (if left 'center 'right) (when left 'left))))

(defrule table-align-row (and #\| (+ table-align-cell) newline)
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
  (should be equal
          '(table (((th "First Header") (th "second Header"))
                   ((td "Content Cell") (td "Content Cell"))
                   ((td "Content Cell") (td "Content Cell"))))
          (parse-doc "
| First Header  | Second Header |
| ------------- | ------------- |
| Content Cell  | Content Cell  |
| Content Cell  | Content Cell  |
"))
  (should be equal
          '(table (((th "Name") (th "Description"))
                   ((td "Help") (td "Display the help window."))
                   ((td "Close") (td "Closes a window"))))
          (parse-doc "
| Name | Description          |
| ------------- | ----------- |
| Help      | Display the help window.|
| Close     | Closes a window     |
"))  (should be equal
          '(table (((th "Left-Aligned" left)
                    (th "Center Aligned" center)
                    (th "Right Aligned" right))
                   ((td "col 3 is" left)
                    (td "some wordy text" center)
                    (td "$1600" right))
                   ((td "col 2 is" left)
                    (td "centered" center)
                    (td "$12" right))
                   ((td "zebra stripes" left)
                    (td "are neat" center)
                    (td "$1" right))))
          (parse-doc "
| Left-Aligned  | Center Aligned  | Right Aligned |
| :------------ |:---------------:| -----:|
| col 3 is      | some wordy text | $1600 |
| col 2 is      | centered        |   $12 |
| zebra stripes | are neat        |    $1 |
"))
)
)
