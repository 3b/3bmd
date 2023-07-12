(in-package #:3bmd-tests)


(def-grammar-test atx-heading-test-1
  :rule 3bmd-grammar::heading
  :text "# Hello Lisp World!
"
  :expected '(:HEADING :LEVEL 1 :CONTENTS ("Hello" " " "Lisp" " " "World" "!")))

(def-grammar-test atx-heading-test-2
  :rule 3bmd-grammar::heading
  :text "### Hello Lisp World!
"
  :expected '(:HEADING :LEVEL 3 :CONTENTS ("Hello" " " "Lisp" " " "World" "!")))


(def-grammar-test atx-heading-nested-in-a-list
  :rule 3bmd-grammar::doc
  :text "* First line

  # The header
"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "First" " " "line")))
              ;; TODO: This should be a header, nested in the previous LIST-ITEM
              (:PLAIN "  " "#" " " "The" " " "header")))


;;;;;;;;;;;;;;;;;;; SETEXT HEADINGS ;;;;;;;;;;;;;;;;;;;

(def-grammar-test setext-heading-test-1
  :rule 3bmd-grammar::heading
  :text "Hello Lisp World!
===
"
  :expected '(:HEADING :LEVEL 1 :CONTENTS ("Hello" " " "Lisp" " " "World" "!")))

(def-grammar-test setext-heading-test-2
  :rule 3bmd-grammar::heading
  :text "Hello Lisp World!
---
"
  :expected '(:HEADING :LEVEL 2 :CONTENTS ("Hello" " " "Lisp" " " "World" "!")))

(def-grammar-test setext-heading-test-3
  :rule 3bmd-grammar::heading
  :text "Hello Lisp World!
==============
"
  :expected '(:HEADING :LEVEL 1 :CONTENTS ("Hello" " " "Lisp" " " "World" "!")))


(def-grammar-test setext-heading-multiline
  :rule 3bmd-grammar::heading
  :text "Hello
Lisp
World!
=====
"
  ;; TODO: Multiline headers from CommonMark are not supported yet
  :fail-expected 'esrap:esrap-parse-error)

(def-grammar-test setext-heading-nested-in-a-list
  :rule 3bmd-grammar::doc
  :text "* First line

  The header
  ==========
"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "First" " " "line")))
              (:PLAIN "  " "The" " " "header" "
"
               "  " "==========")))

(def-grammar-test atx-heading-in-a-list ;; bug 35
  ;;; marked bug as invalid, since original markdown is inconsistent
  ;;; about parsing headings in lists, but commonmark says a space is
  ;;; required after # to be a heading, which we don't match, so
  ;;; leaving this here
  :text "* #foo
* # foo
* ##bar
* ## bar
"
  :known-failure t
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "#foo"))
               (:LIST-ITEM (:HEADING :LEVEL 1 :CONTENTS ("foo")))
               (:LIST-ITEM (:PLAIN "##bar"))
               (:LIST-ITEM (:HEADING :LEVEL 2 :CONTENTS ("bar"))))))

(def-grammar-test atx-heading-test-1b
  ;; todo: commonmark says this shouldn't be a heading, see above
  :text "#Hello Lisp World!
"
  :known-failure t
  :expected '(:PLAIN "#Hello" " " "Lisp" " " "World" "!"))

(def-grammar-test atx-heading-eof-test
  :text "# heading"
  :expected '((:HEADING :LEVEL 1 :CONTENTS ("heading"))))
