(in-package #:3bmd-grammar)

;;; expand '," to smart quotes, ... to ellipsis, etc
(defparameter *smart-quotes* nil)
;;; [[foo|bar]] style wiki links
;;; parsed as a link label followed by 0 or more args separated by |
;;; actual interpretation is up to the printing layer
(defparameter *wiki-links* nil)
;; todo: footnotes...

(defparameter *footnotes* nil)

(defrule eof (! character)
  (:constant ""))
(defrule space-char (or #\space #\tab)
  (:text t))
(defrule newline (or #\linefeed (and #\return #\linefeed))
  (:text t))
(defrule sp (* space-char)
  (:text t))
(defrule spnl (and sp (? (and newline sp)))
  (:text t))
(defrule blank-line (and sp newline)
  (:constant "
"))

(defrule nonindent-space (or "   " "  " " " "")
  (:text t))
(defrule indent (or #\tab "    ")
  (:constant "    "))
(defrule normal-endline (and sp newline
                             ;; newline not followed by a blank line, >,
                             ;; ## heading, or ===/___ heading
                             (! blank-line) (! #\>) (! atx-start)
                             (! (and line
                                     (or (and "===" (* #\=))
                                         (and "___" (* #\_)))
                                     newline)))
  (:text t))
(defrule terminal-endline (and sp newline eof)
  (:text t))

(defrule line-break (and "  " normal-endline)
  (:constant '(:line-break)))
(defrule endline (or line-break terminal-endline normal-endline))
(defrule normal-char (and (! (or special-char space-char newline)) character)
  (:text t))
(defrule special-char (or #\* #\_ #\` #\& #\[ #\] #\< #\! #\# #\\
                          extended-special-char)
  (:text t))
;; fixme: if possible, would be nice to refactor this to avoid duplicates
;;  when more than one extension is enabled
(defrule smart-quote-extended-chars (or #\. #\- #\' #\" #\=)
  (:when *smart-quotes*))
(defrule wiki-link-extended-chars (or #\| #\= #\')
  (:when *wiki-links*))
(defrule notes-extended-chars #\^
  (:when *footnotes*))
(defrule extended-special-char (or smart-quote-extended-chars
                                   wiki-link-extended-chars
                                   notes-extended-chars)
  (:text t))
(defrule non-space-char (and (! space-char) (! newline) character)
  (:text t))
(defrule alphanumeric (alphanumericp character))
(defrule dec-digit (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defrule hex-digit (or dec-digit
                       #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F))
(defun ascii-char-p (c)
  (let ((c (char-code c)))
    (or (<= (char-code #\a) c (char-code #\z))
        (<= (char-code #\A) c (char-code #\Z))
        (<= (char-code #\0) c (char-code #\9)))))
(defrule |A-Za-z| #.`(or ,@(coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'list)
                         ,@(coerce "abcdefghijklmnopqrstuvwxyz" 'list)))
(defrule ascii-character (ascii-char-p character))
(defrule alphanumeric-ascii (ascii-char-p character))



(defrule doc (* block))

(defrule block (and (* blank-line)
                    (or block-quote
                        verbatim
                        #++ note
                        reference
                        horizontal-rule
                        heading
                        ordered-list
                        bullet-list
                        html-block
                        #++ style-block
                        paragraph
                        plain
                        ))
  (:destructure (blank block)
                (declare (ignore blank))
                block))

(defun parse-doc (a)
  #++(parse 'doc a)
  ;; ~2-5x faster and much less ram use on a big document than parsing 'doc
  ;; directly, not sure it is 100% equivalent though
  (loop
     for start = 0 then pos
     for (block pos) = (multiple-value-list (parse 'block a
                                                   :start start
                                                   :junk-allowed t))
     while block
     collect block
     while pos))


(defrule line raw-line
  (:text t))
(defrule raw-line (or (and (* (and (! #\newline) (! #\return) character))
                           newline)
                      (and (+ character) eof) ))
(defrule optionally-indented-line (and (? indent) line)
  (:destructure (i l)
                (declare (ignore i))
                l))

(defrule block-quote (+ (and (and #\> (? #\space) line)
                             (* (and (! #\>) (! blank-line) line))
                             (* blank-line)))
  (:destructure (&rest chunks)
                (cons :block-quote
                      (parse-doc
                       (text
                        (loop for ((> sp l1) (lines) (blank)) in chunks
                           collect l1
                           collect lines
                           collect blank)
                        "

")))))


(defrule indented-line (and indent line)
  (:destructure (i line)
                (declare (ignore i))
                line))
(defrule non-blank-indented-line (and (! blank-line) indented-line))
(defrule verbatim-chunk (and ;;fixme: indented blank lines with whitespace
                         ;; "\t \n" or "     \n" -> " \n" instead of "\n"
                         (* blank-line)
                             (+ non-blank-indented-line)))
(defrule verbatim (+ verbatim-chunk)
  (:lambda (a)
    (list :verbatim (text a))))


(defrule ref-title (or ref-title-single ref-title-double ref-title-parens empty-title))
(macrolet ((def-ref-title (name delim &optional (close-delim delim))
             `(defrule ,name (and ,delim
                                  (* (and (! (or (and ,close-delim sp newline)
                                                 (and ,close-delim eof)
                                                 newline))
                                          character))
                                  ,close-delim)
                (:destructure (q title q2)
                              (declare (ignore q q2))
                              (text title)))))
  (def-ref-title ref-title-single #\')
  (def-ref-title ref-title-double #\")
  (def-ref-title ref-title-parens #\( #\)))
(defrule empty-title ""
  (:constant nil))

(defrule ref-source (+ non-space-char)
  (:text t))

;; fixme: is 'label' field allowed to have markup?
;; peg-markdown seems to allow it, but markdown.pl doesn't
(defrule reference (and nonindent-space (! "[]") label #\: spnl ref-source
                        spnl ref-title (* blank-line))
  (:destructure (s n label \: s2 source s3 title b)
                (declare (ignore s n \: s2 s3 b))
                (list :reference :label label :source source :title title)))

(defrule horizontal-rule (and nonindent-space
                              (or (and #\* sp #\* sp #\* (* (and sp #\*)))
                                  (and #\- sp #\- sp #\- (* (and sp #\-)))
                                  (and #\_ sp #\_ sp #\_ (* (and sp #\_))))
                              sp newline blank-line)
  (:constant '(:horizontal-rule)))



(defrule heading (or atx-heading setext-heading))

(defrule atx-heading (and atx-start sp (+ atx-inline)
                          (? (and sp (* #\#) sp))
                          newline)
  (:destructure (start s contents s2 nl)
                (declare (ignore s s2 nl))
                (append start (list :contents contents))))

(defrule atx-start (or "######" "#####" "####" "###" "##" "#")
  ;; :destructure doesn't work on (or ...)
  (:lambda (a)
    (list :heading :level (length a))))

(defrule atx-inline (and (! newline) (! (and sp (* #\#) sp newline)) inline)
  (:destructure (n s inline)
                (declare (ignore n s))
                inline))

(defrule setext-heading (or setext-heading-1 setext-heading-2))

(defrule setext-bottom-1 (and "===" (* #\=) newline))
(defrule setext-bottom-2 (and "---" (* #\-) newline))

(defrule setext-heading-1 (and (& (and raw-line setext-bottom-1))
                               (+ (and (! endline) inline))
                               sp newline
                               setext-bottom-1)
  (:destructure (& content s nl line)
                (declare (ignore & s nl line))
                (list :heading :level 1 :contents (mapcar 'second content))))

(defrule setext-heading-2 (and (& (and raw-line setext-bottom-2))
                               (+ (and (! endline) inline))
                               sp newline
                               setext-bottom-2)
  (:destructure (& content s nl line)
                (declare (ignore & s nl line))
                (list :heading :level 2 :contents (mapcar 'second content))))


(defrule bullet (and (! horizontal-rule) nonindent-space
                     (or #\+ #\* #\-)
                     (+ space-char))
  (:constant :bullet-list))
(defrule bullet-list (and (& bullet) (or list-tight list-loose))
  (:destructure (a b)
                (cons a b)))

(defrule enumerator (and nonindent-space (+ dec-digit) #\. (+ space-char))
  (:constant :counted-list))
(defrule ordered-list (and (& enumerator) (or list-tight list-loose))
  (:destructure (a b)
                (cons a b)))


(defrule list-tight (and (+ list-item-tight)
                         (* blank-line)
                         (! (or bullet enumerator)))
  (:destructure (items i1 i2)
                (declare (ignore i1 i2))
                items))

(defrule list-item-tight (and (or bullet enumerator)
                              list-block
                              (* (and (! blank-line)
                                      list-continuation-block))
                              (! list-continuation-block))
  (:destructure (b block cont e)
                (declare (ignore b e))
                (list* :list-item
                       (mapcan (lambda (a)
                                 (parse-doc (text a)))
                               (split-sequence:split-sequence
                                :split (cons block (mapcan 'second cont))
                                :remove-empty-subseqs t)))))

(defrule list-loose (and (+ (and list-item (* blank-line))))
  (:destructure (items)
                (mapcar 'first items)))

(defrule list-item (and (or bullet enumerator)
                        list-block
                        (* list-continuation-block))
  (:destructure (b block cont)
                (declare (ignore b))
                (list* :list-item
                       (mapcan (lambda (a)
                                 (parse-doc (text a)))
                               (split-sequence:split-sequence
                                :split (append (cons block (mapcan 'identity cont))
                                               (list "

"))
                                :remove-empty-subseqs t)))))

(defrule list-block (and (! blank-line)
                         line
                         (* list-block-line))
  (:destructure (b l block)
                (declare (ignore b))
                (text l block)))

(defrule list-continuation-block (and (* blank-line)
                                      (+ (and indent list-block)))
  (:destructure (b c)
                (if b
                    (cons (text b) (mapcar 'second c))
                    (cons :split (mapcar 'second c)))))

(defrule list-block-line (and (! blank-line)
                              (! (and (? indent) (or bullet enumerator)))
                              (! horizontal-rule)
                              optionally-indented-line)
  (:destructure (i1 i2 i3 line)
                (declare (ignore i1 i2 i3))
                line))



(defrule html-block (and (or html-block-in-tags html-comment
                             html-block-self-closing
                             html-block-non-closing)
                         (+ blank-line))
  (:destructure (html b)
                (declare (ignore b))
                (list :html (text html))))

(defrule html-block-self-closing (and #\< spnl
                                      html-block-type spnl
                                      (* html-attribute)
                                      #\/ spnl
                                      #\>))

(defrule html-block-type (or "address" "blockquote" "center" "dir" "div" "dl"
                             "fieldset" "form" "h1" "h2" "h3" "h4" "h5" "h6"
                             "hr" "isindex" "menu" "noframes" "noscript" "ol"
                             "pre" "p" "table" "ul" "dd" "dt" "frameset" "li"
                             "tbody" "td" "tfoot" "thead" "th" "tr" "script"
                             "ADDRESS" "BLOCKQUOTE" "CENTER" "DIR" "DIV" "DL"
                             "FIELDSET" "FORM" "H1" "H2" "H3" "H4" "H5" "H6"
                             "HR" "ISINDEX" "MENU" "NOFRAMES" "NOSCRIPT" "OL"
                             "PRE" "P" "TABLE" "UL" "DD" "DT" "FRAMESET" "LI"
                             "TBODY" "TD" "TFOOT" "THEAD" "TH" "TR" "SCRIPT"))

(defrule html-block-non-closing (and #\< spnl
                                     (or "hr" "HR")
                                     spnl
                                     (* html-attribute)
                                     spnl
                                     #\>))

(macrolet ((def-html-block (name tag)
             (let* ((tag-case `(or ,(string-downcase tag)
                                   ,(string-upcase tag)))
                    (end `(and #\< spnl #\/ ,tag-case spnl #\>)))
               `(defrule ,name (and #\< spnl
                                    ,tag-case spnl
                                    (* html-attribute) spnl
                                    #\>
                                    (* (or ,name
                                           (and (! ,end) character)))
                                    ,end)
                  (:text t)))))
  (def-html-block html-block-address "address")
  (def-html-block html-block-blockquote "blockquote")
  (def-html-block html-block-center "center")
  (def-html-block html-block-dir "dir")
  (def-html-block html-block-div "div")
  (def-html-block html-block-dl "dl")
  (def-html-block html-block-fieldset "fieldset")
  (def-html-block html-block-form "form")
  (def-html-block html-block-h1 "h1")
  (def-html-block html-block-h2 "h2")
  (def-html-block html-block-h3 "h3")
  (def-html-block html-block-h4 "h4")
  (def-html-block html-block-h5 "h5")
  (def-html-block html-block-h6 "h6")
  (def-html-block html-block-menu "menu")
  (def-html-block html-block-noframes "noframes")
  (def-html-block html-block-noscript "noscript")
  (def-html-block html-block-ol "ol")
  (def-html-block html-block-p "p")
  (def-html-block html-block-pre "pre")
  (def-html-block html-block-table "table")
  (def-html-block html-block-ul "ul")
  (def-html-block html-block-dd "dd")
  (def-html-block html-block-dt "dt")
  (def-html-block html-block-frameset "frameset")
  (def-html-block html-block-li "li")
  (def-html-block html-block-tbody "tbody")
  (def-html-block html-block-td "td")
  (def-html-block html-block-tfoot "tfoot")
  (def-html-block html-block-th "th")
  (def-html-block html-block-thead "thead")
  (def-html-block html-block-tr "tr")
  (def-html-block html-block-script "script"))

(defrule html-block-in-tags (or html-block-address
                                html-block-blockquote
                                html-block-center
                                html-block-dir
                                html-block-div
                                html-block-dl
                                html-block-fieldset
                                html-block-form
                                html-block-h1
                                html-block-h2
                                html-block-h3
                                html-block-h4
                                html-block-h5
                                html-block-h6
                                html-block-menu
                                html-block-noframes
                                html-block-noscript
                                html-block-ol
                                html-block-p
                                html-block-pre
                                html-block-table
                                html-block-ul
                                html-block-dd
                                html-block-dt
                                html-block-frameset
                                html-block-li
                                html-block-tbody
                                html-block-td
                                html-block-tfoot
                                html-block-th
                                html-block-thead
                                html-block-tr
                                html-block-script))

(defrule paragraph (and nonindent-space inlines (+ blank-line))
  (:destructure (space paragraph blank)
                (declare (ignore space blank))
                (cons :paragraph paragraph)))

(defrule inlines (and (+ (or (and (! endline) inline)
                             (and endline (& inline))))
                     (? endline))
  (:destructure (i e)
                (declare (ignore e))
                (mapcar (lambda (a) (or (first a) (second a))) i)))

(defrule plain inlines
  (:lambda (a)
    (cons :plain a)))



(defrule inline (or string
                    endline
                    ul-or-star-line
                    space
                    strong
                    emph
                    quoted-wiki-link
                    wiki-link
                    image
                    link
                    #++ note-reference
                    #++ inline-note
                    code
                    raw-html
                    entity
                    escaped-character
                    smart
                    symbol
                    ))

(defrule maybe-alphanumeric (& alphanumeric)
  (:constant ""))
(defrule string (and normal-char (* (or normal-char (and (+ #\_) maybe-alphanumeric))))
  (:text t))

(defrule maybe-space-char (& space-char)
  (:constant ""))
(defrule ul-or-star-line (or ul-line star-line)
  (:text t))
(defrule star-line (or (and "****" (* #\*)) (and space-char (+ #\*) maybe-space-char)))
(defrule ul-line (or (and "____" (* #\_)) (and space-char (+ #\_) maybe-space-char)))

(defrule space (+ space-char)
  (:text t))

(defrule strong (or strong-star strong-ul)
  (:destructure (&rest a)
                (cons :strong a)))
(defrule **-open (and (! star-line) "**" (! space-char) (! newline)))
(defrule **-close (and (! space-char) (! newline) inline (! star-line) "**" )
  (:destructure (s n inline s2 *s)
                (declare (ignore s n s2 *s))
                inline))
(defrule strong-star (and **-open (* (and (! **-close) inline)) **-close)
  (:destructure (o (&rest i) e)
                (declare (ignore o))
                (append (mapcar 'second i) (list e))))
(defrule __-open (and (! star-line) "__" (! space-char) (! newline)))
(defrule __-close (and (! space-char) (! newline) inline (! star-line) "__" )
  (:destructure (s n inline s2 _s)
                (declare (ignore s n s2 _s))
                inline))
(defrule strong-ul (and __-open (* (and (! __-close) inline)) __-close)
  (:destructure (o (&rest i) e)
                (declare (ignore o))
                (append (mapcar 'second i) (list e))))

(defrule emph (or emph-star emph-ul)
  (:destructure (&rest a)
                (cons :emph a)))
(defrule *-open (and (! star-line) "*" (! space-char) (! newline)))
(defrule *-close (and (! space-char) (! newline) inline (! star-line) "*" )
  (:destructure (s n inline s2 *s)
                (declare (ignore s n s2 *s))
                inline))
(defrule emph-star (and *-open (* (and (! *-close) inline)) *-close)
  (:destructure (o (&rest i) e)
                (declare (ignore o))
                (append (mapcar 'second i) (list e))))
(defrule _-open (and (! star-line) "_" (! space-char) (! newline)))
(defrule _-close (and (! space-char) (! newline) inline (! star-line) "_" )
  (:destructure (s n inline s2 _s)
                (declare (ignore s n s2 _s))
                inline))
(defrule emph-ul (and _-open (* (and (! _-close) inline)) _-close)
  (:destructure (o (&rest i) e)
                (declare (ignore o))
                (append (mapcar 'second i) (list e))))


;;; allowing markup for now, to be normalized like ref links during printing...
(defrule wiki-link-label (* (and (! #\]) (! #\|) inline))
  (:lambda (a)
    (mapcar 'third a)))

(defrule wiki-link-arg (* (and (! "|") (! "]]") character))
  (:text t))
(defrule wiki-link (and "[["
                        wiki-link-label
                        (* (and "|" wiki-link-arg))
                        "]]")
  (:when *wiki-links*)
  (:destructure ([ label args ])
                (declare (ignore [ ]))
                (list :wiki-link :label label :args (mapcar 'second args))))

(defrule quoted-wiki-link (and #\'
                               ;; would be nicer to just use wiki-link
                               ;; rule rather than duplicating it
                               ;; here, but then we'd have to
                               ;; serialize it back to text to put the
                               ;; "[[" back, and worry about
                               ;; whitespace, etc
                               "[["
                               (* (and (! #\]) character))
                               "]]")
  (:when *wiki-links*)
  (:destructure (q &rest link)
                (declare (ignore q))
                (text link)))

(defrule image (and #\! (or explicit-link reference-link))
  (:destructure (! link)
                (declare (ignore !))
                (list :image link)))
(defrule link (or explicit-link reference-link auto-link))

(defrule reference-link (or reference-link-double reference-link-single))
(defrule reference-link-double (and label spnl (! "[]") label)
  (:destructure (link i1 i2 definition)
                (declare (ignore i1 i2))
                (list :reference-link :label link
                      :definition (text definition))))
(defrule reference-link-single (and label (? (and spnl "[]")))
  (:destructure (label tail)
                (let ((tail (text tail)))
                  (list :reference-link :label label
                        :tail (if (string= tail "") nil tail)))))
(defrule explicit-link (and label spnl "(" sp source spnl title sp ")")
  (:destructure (label n n2 n3 source n4 title s n5)
                (declare (ignore n n2 n3 n4 n5 s))
                (list :explicit-link :label label
                      :source (text source)
                      :title (if (equal title "") nil (text title)))))

(defrule auto-link (or auto-link-url auto-link-email))
(defrule auto-link-url (and #\< (+ |A-Za-z|) "://" (+ (and (! newline)
                                                           (! #\>)
                                                           character))
                            #\>)
  (:destructure (< url1 url2 url3 >)
                (declare (ignore < >))
                (list :link (text url1 url2 url3))))
(defrule auto-link-email (and #\< (+ ascii-character) "@" (+ (and (! newline)
                                                           (! #\>)
                                                           character))
                            #\>)
  (:destructure (< url1 url2 url3 >)
                (declare (ignore < >))
                (list :link (text "mailto:" url1 url2 url3))))

(defrule label (and #\[ (* (and (! #\]) inline)) #\])
  (:destructure ([ label ])
                (declare (ignore [ ]))
                (mapcar 'second label)))

(defrule source (or (and "<" source-contents ">") source-contents)
  (:lambda (a)
    (if (and (consp a) (eql (car a) "<"))
        (progn (format t "link with < -> ~s~%" (second a)) (second a))
        a)))
(defrule source-contents (or (* (or (+ (and (! "(") (! ")") (! #\>) non-space-char))
                                    (and "(" source-contents ")")))
                             "")
  (:lambda (a)
    (text a)))
(defrule title (or title-single title-double ""))
(defrule title-single (and #\' (* (and (! (and #\' sp (or #\) newline)))
                                       character))
                           #\')
  (:destructure (q1 a q2)
                (declare (ignore q1 q2))
                (text a)))
(defrule title-double (and #\" (* (and (! (and #\" sp (or #\) newline)))
                                       character))
                           #\")
  (:destructure (q1 a q2)
                (declare (ignore q1 q2))
                (text a)))

(macrolet
    ((ticks-code (ticks code str)
       `(progn
          (defrule ,ticks (and ,str (! "`")))
          (defrule ,code (and ,ticks sp
                              (+ (or (+ (and (! #\`) non-space-char))
                                     (and (! ,ticks) (+ #\`))
                                     (and (! (and sp ,ticks))
                                          (or space-char
                                              (and newline (! blank-line))))))
                              sp ,ticks)
            (:destructure (q n a n2 q2)
                          (declare (ignore q n n2 q2))
                          (text a))))))
  (ticks-code ticks1 code1 "`")
  (ticks-code ticks2 code2 "``")
  (ticks-code ticks3 code3 "```")
  (ticks-code ticks4 code4 "````")
  (ticks-code ticks5 code5 "`````"))
(defrule code (or code1 code2 code3 code4 code5)
  (:lambda (a)
    (list :code a)))


(defrule raw-html (or html-comment html-tag)
  (:lambda (a)
    (list :raw-html a)))
(defrule html-comment (and "<!--" (* (and (! "-->") character)) "-->")
  (:text t))
(defrule html-tag (and #\< spnl (? #\/) (+ alphanumeric-ascii)
                       spnl (* html-attribute)
                       (? #\/) #\>)
  (:text t))
(defrule html-attribute (and (+ (or alphanumeric-ascii #\-)) spnl
                             (? (and "=" spnl (or quoted
                                                  (+ (and (! (or #\> #\' #\"))
                                                          non-space-char)))))
                             spnl))
(defrule quoted (or (and #\" (+ (and (! #\") character)) #\")
                    (and #\' (+ (and (! #\') character)) #\')))

(defrule entity (or hex-entity dec-entity char-entity)
  (:lambda (a)
    (list :entity (text a))))
(defrule hex-entity (and (~ "&#x") (+ hex-digit) ";")
  (:text t))
(defrule dec-entity (and "&#" (+ dec-digit) ";")
  (:text t))
(defrule char-entity (and "&" (+ ascii-character) ";")
  (:text t))


(defrule smart-quote-escaped-characters #.`(or ,@(coerce "=\"'" 'list))
         (:when *smart-quotes*))
(defrule escaped-character (and #\\ (! newline)
                                #.`(or ,@(coerce  "-\\`|*_{}[]()#+.!<>" 'list)
                                       smart-quote-escaped-characters))
  (:destructure (\\ n c)
                (declare (ignore \\ n))
                (if (consp c) (second c) c)))

(defrule smart (or ellipsis dash single-quoted double-quoted
        apostrophe
        arrows)
  (:when *smart-quotes*)
  (:lambda (value)
    value))

;; we store the original text for ellipsis,dashes,etc so we can use it inside
;; code blocks
(defrule ellipsis (or "..." ". . .")
  (:lambda (a) (list :ellipsis a)))

(defrule dash (or em-dash en-dash))
#++
(defrule em-dash (or "---" "--")
  (:lambda (a) (list :em-dash a)))
#++
(defrule en-dash (and "-" (& dec-digit))
  (:lambda (a) (cons :en-dash a)))
;; not sure what the - &digit thing was trying to do, so just doing
;; "--" -> en-dash and "---" -> em-dash instead, and "-" can stay as-is
(defrule em-dash "---"
  (:lambda (a) (list :em-dash a)))
(defrule en-dash "--"
  (:lambda (a) (cons :en-dash a)))

(defrule single-quote-start (and #\'
                                 (! #.`(or ,@(coerce ")!],.;:-?" 'list)
                                           #\space #\tab #\newline
                                           #\return))
                                 (! (and (or "s" "t" "m" "ve" "ll" "rr")
                                         (! alphanumeric))))
  (:constant ""))
(defrule single-quote-end (and #\' (! alphanumeric))
  (:constant ""))
(defrule single-quoted (and single-quote-start
                            (+ (and (! single-quote-end)
                                    inline))
                            single-quote-end)
  (:destructure (q content q2)
                (declare (ignore q q2))
    (cons :single-quoted (mapcar 'second content))))


(defrule double-quote-start #\"
  (:constant ""))
(defrule double-quote-end #\"
  (:constant ""))
(defrule double-quoted (and double-quote-start
                            (+ (and (! double-quote-end)
                                    inline))
                            double-quote-end)
  (:destructure (q content q2)
                (declare (ignore q q2))
    (cons :double-quoted (mapcar 'second content))))

(defrule apostrophe #\'
  (:constant :apostrophe))

(macrolet ((define-arrows (name pattern)
             `(defrule ,name ,pattern
                (:lambda (a) (list ,(alexandria:make-keyword name) a)))))
  (define-arrows left-right-single-arrow "<->")
  (define-arrows left-single-arrow "<-")
  (define-arrows right-single-arrow "->")
  (define-arrows left-right-double-arrow "<=>")
  (define-arrows left-double-arrow "<=")
  (define-arrows right-double-arrow "=>"))

(defrule arrows (or left-right-single-arrow left-single-arrow right-single-arrow
                    left-right-double-arrow left-double-arrow right-double-arrow))




(defrule symbol special-char)
