(in-package 3bmd-tests)


(def-grammar-test string-test-1
  :rule 3bmd-grammar::string
  :text "Some string with spaces."
  :expected "Some"
  :remaining-text " string with spaces.")

(def-grammar-test string-test-2
  :rule 3bmd-grammar::string
  :text "100500 string with spaces."
  :expected "100500"
  :remaining-text " string with spaces.")

(def-grammar-test string-test-3
  :rule 3bmd-grammar::string
  :text "@a_symbol@ string with spaces."
  :expected "@a_symbol@"
  :remaining-text " string with spaces.")

(def-grammar-test string-test-4
  :rule 3bmd-grammar::string
  :text "*a_symbol* string with spaces."
  ;; TODO: Probably this is not an error, because * surrounds
  ;; emphasised text.
  :expected nil
  :parse-should-succeed nil
  :remaining-text "*a_symbol* string with spaces.")
