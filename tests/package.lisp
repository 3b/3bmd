(fiasco:define-test-package #:3bmd-tests)

(in-package 3bmd-tests)


(defmacro def-grammar-test (name &key
                                   (rule '3bmd-grammar::doc)
                                   text
                                   expected
                                   (fail-expected nil)
                                   (no-match nil)
                                   (junk-allowed nil)
                                   (remaining-text nil))
  (let ((expected-remaining-text remaining-text))
    `(deftest ,name ()
       (let ((expected ,expected)
             (catched-condition nil))
         (multiple-value-bind (result remaining-text-start parse-succeeded)
             (block parser-call
               (handler-bind
                   ((error (lambda (c)
                             (when ,fail-expected
                               (setf catched-condition c)
                               (return-from parser-call
                                 (values nil))))))
                 (esrap:parse ',rule  ,text
                              :junk-allowed ,expected-remaining-text)))
           (cond
             ((and (null ,fail-expected)
                   (null ,no-match))
              (is (equalp result expected))
              (if ,expected-remaining-text
                  (is (string= (subseq ,text remaining-text-start)
                               ,expected-remaining-text))
                  (is (not remaining-text-start)))
              (is parse-succeeded))
             (,fail-expected
              (is (typep catched-condition
                         ,fail-expected)))
             (t
              (is (not parse-succeeded)))))))))
