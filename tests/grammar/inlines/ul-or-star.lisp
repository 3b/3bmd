(in-package 3bmd-tests)


(def-grammar-test ul-line-1
  :rule 3bmd-grammar::ul-or-star-line
  :text "____Some other text"
  :expected "____"
  :remaining-text "Some other text")

(def-grammar-test ul-line-2
  :rule 3bmd-grammar::ul-or-star-line
  ;; A number of underlines could be more than 4 chars:
  :text "______ Some other text"
  :expected "______"
  :remaining-text " Some other text")

(def-grammar-test ul-line-3
  :rule 3bmd-grammar::ul-or-star-line
  ;; TODO: investigate, why this should be considered an underline.
  :text " _ Some other text"
  :expected " _"
  :remaining-text " Some other text")

(def-grammar-test ul-line-4
  :rule 3bmd-grammar::ul-or-star-line
  ;; TODO: investigate, why this should not be considered an underline.
  :text " _Some other text"
  :fail-expected 'esrap:esrap-parse-error)

(def-grammar-test ul-line-5
  :rule 3bmd-grammar::ul-or-star-line
  ;; Also, this work for more underline characters:
  :text " ___ Some other text"
  :expected " ___"
  :remaining-text " Some other text")


;; The same for stars


(def-grammar-test star-line-1
  :rule 3bmd-grammar::ul-or-star-line
  :text "****Some other text"
  :expected "****"
  :remaining-text "Some other text")

(def-grammar-test star-line-2
  :rule 3bmd-grammar::ul-or-star-line
  ;; A length of starslines could be more than 4 chars:
  :text "****** Some other text"
  :expected "******"
  :remaining-text " Some other text")

(def-grammar-test star-line-3
  :rule 3bmd-grammar::ul-or-star-line
  ;; TODO: investigate, why this should be considered an starline.
  :text " * Some other text"
  :expected " *"
  :remaining-text " Some other text")

(def-grammar-test star-line-4
  :rule 3bmd-grammar::ul-or-star-line
  ;; TODO: investigate, why this should not be considered an starline.
  :text " *Some other text"
  :fail-expected 'esrap:esrap-parse-error)

(def-grammar-test star-line-5
  :rule 3bmd-grammar::ul-or-star-line
  ;; Also, this work for more star characters:
  :text " *** Some other text"
  :expected " ***"
  :remaining-text " Some other text")
