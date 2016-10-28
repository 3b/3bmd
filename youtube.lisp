;-------------------------------------------------------------------------------
; Embed youtube videos
; Author: Lukasz Janyst <lukasz@jany.st>
;
; !yt[id]
; !yt[id|width=100,height=200,allowfullscreen]
;-------------------------------------------------------------------------------

(defpackage #:3bmd-youtube
  (:use :cl :esrap :3bmd-ext)
  (:export #:*youtube-embeds*))

(in-package #:3bmd-youtube)

(defrule yt-param-value
    (and "="
         (+ (and (! "]") (! ",") character)))
  (:destructure (e v)
                (declare (ignore e))
                (text v)))

(defrule yt-param
    (and (or "|" ",")
         (+ (and (! "=") (! "]") (! ",") character))
         (? yt-param-value))
  (:destructure (delim name value)
                (declare (ignore delim))
                (cons (text name) value)))

(define-extension-inline *youtube-embeds* youtube-embed
    (and "!yt["
         (* (and (! "]") (! "|") character))
         (? (+ yt-param))
         "]")
  (:destructure (s id params e)
                (declare (ignore s e))
                (append
                 (list :youtube-embed (text id))
                 params)))

(defmethod print-tagged-element ((tag (eql :youtube-embed)) stream rest)
  (let ((id (car rest))
        (params (cdr rest)))
    (format stream
            (concatenate
             'string
             "<iframe src=\"https://www.youtube.com/embed/~a\" "
             "frameborder=\"0\" "
             "~{~a~^ ~}"
             "></iframe>")
            id
            (mapcar (lambda (x)
                      (format nil "~a~@[=\"~a\"~]" (car x) (cdr x)))
                    params))))
