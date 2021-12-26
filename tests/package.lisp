(fiasco:define-test-package #:3bmd-tests)


(defmacro def-grammar-test (name &key
                                   (rule '3bmd-grammar::doc)
                                   text
                                   expected)
  `(deftest ,name ()
     (let ((expected ,expected))
       (multiple-value-bind (result remaining-text parse-succeeded)
           (esrap:parse ',rule  ,text)
         (is (equalp result expected))
         (is (not remaining-text))
         (is parse-succeeded)))))
