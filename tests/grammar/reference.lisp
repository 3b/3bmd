(in-package #:3bmd-tests)


(def-grammar-test reference-block
  :rule 3bmd-grammar::reference
  :text "[foo]: /url \"the title\""
  :expected '(:REFERENCE :LABEL ("foo") :SOURCE "/url" :TITLE "the title"))


(def-grammar-test reference-block-with-spaces
  :rule 3bmd-grammar::reference
  :text "[foo]: 
      /url  
           'the title'"
  :expected '(:REFERENCE :LABEL ("foo") :SOURCE "/url" :TITLE "the title"))
