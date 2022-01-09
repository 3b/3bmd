(in-package 3bmd-tests)

;; Images are share most rules with images
;; Thus this file contains tests only for auto-links.

(def-grammar-test auto-link-test-1
  :rule 3bmd-grammar::link
  :text "<http://placekitten.com/300/200>"
  :expected '(:LINK "http://placekitten.com/300/200"))

(def-grammar-test auto-link-test-2
  :rule 3bmd-grammar::link
  ;; Scheme could be any
  :text "<sftp://some.net/?user=moot>"
  :expected '(:LINK "sftp://some.net/?user=moot"))

(def-grammar-test auto-link-test-3
  :rule 3bmd-grammar::link
  ;; Scheme could be any
  :text "<sasha@svetlyak.ru>"
  ;; TODO: probably should add "mailto:" prefix
  ;; only when rendering a link into the HTML?
  :expected '(:MAILTO "mailto:sasha@svetlyak.ru"))

(def-grammar-test auto-link-test-4
  :rule 3bmd-grammar::link
  ;; Scheme could be any
  :text "<common-lisp@svetlyak.ru>"
  ;; TODO:
  ;; Right now LINK rule fails with the error
  ;; The production
  ;;   #\-
  ;; does not satisfy the predicate 3BMD-GRAMMAR::ASCII-CHAR-P.
  ;;
  ;; Seems this is a bug and more complex regular expression
  ;; should be used to match all possible emails
  :fail-expected 'esrap:esrap-parse-error)
