(fiasco:define-test-package #:3bmd-youtube-tests
  (:use #:3bmd-youtube))

(in-package #:3bmd-youtube-tests)

(deftest parse-without-options ()
  (let ((3bmd-youtube:*youtube-embeds* t))
    (multiple-value-bind (production remaining-text parse-succeeded)
        (esrap:parse '%inline "!yt[nbY-meOL57I]")
      (is (equalp production '(:youtube-embed "nbY-meOL57I")))
      (is (not remaining-text))
      (is parse-succeeded))))

(deftest parse-with-one-option ()
  (let ((3bmd-youtube:*youtube-embeds* t))
    (multiple-value-bind (production remaining-text parse-succeeded)
        (esrap:parse '%inline "!yt[nbY-meOL57I|width=600]")
      (is (equalp production '(:youtube-embed "nbY-meOL57I" ("width" . "600"))))
      (is (not remaining-text))
      (is parse-succeeded))))

(deftest parse-with-options ()
  (let ((3bmd-youtube:*youtube-embeds* t))
    (multiple-value-bind (production remaining-text parse-succeeded)
        (esrap:parse '%inline "!yt[nbY-meOL57I|width=600,allowfullscreen]")
      ;; TODO: The test shouldn't depend on the order of options.
      (is (equalp production '(:youtube-embed "nbY-meOL57I" ("width" . "600") ("allowfullscreen"))))
      (is (not remaining-text))
      (is parse-succeeded))))



(deftest expand-to-iframe-without-options ()
  (let ((3bmd-youtube:*youtube-embeds* t))
    (let ((output (with-output-to-string (s)
                    (3bmd:parse-string-and-print-to-stream "!yt[nbY-meOL57I]" s))))
      (is (string= "<p><iframe src=\"https://www.youtube.com/embed/nbY-meOL57I\" frameborder=\"0\" ></iframe></p>
"
                   output)))))

(deftest expand-to-iframe-with-options ()
  (let ((3bmd-youtube:*youtube-embeds* t))
    (let ((output (with-output-to-string (s)
                    (3bmd:parse-string-and-print-to-stream "!yt[nbY-meOL57I|width=20,allowfullscreen]" s))))
      (is (string= "<p><iframe src=\"https://www.youtube.com/embed/nbY-meOL57I\" frameborder=\"0\" width=\"20\" allowfullscreen></iframe></p>
"
                   output)))))
