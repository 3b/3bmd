(fiasco:define-test-package #:3bmd-wiki-links-tests)

(in-package #:3bmd-wiki-links-tests)


(3bmd-tests::def-grammar-test wiki-links-1
  :enable-extensions 3bmd-wiki:*wiki-links*
  :rule 3bmd-grammar::%inline
  :text "[[foo|bar]]"
  :expected '(3bmd-wiki::wiki-link :label ("foo") :args ("bar")))

(3bmd-tests::def-grammar-test wiki-links-2
  :enable-extensions 3bmd-wiki:*wiki-links*
  :rule 3bmd-grammar::%inline
  :text "'[[foo|bar]]"
  :expected '"[[foo|bar]]")

