(fiasco:define-test-package #:3bmd-tests)

(in-package 3bmd-tests)


(defmacro def-grammar-test (name &key
                                   (rule '3bmd-grammar::doc)
                                   text
                                   expected
                                   (fail-expected nil)
                                   (parse-should-succeed t)
                                   (no-match nil)
                                   (remaining-text nil)
                                   (enable-extensions nil)
                                   known-failure)
  (let ((expected-remaining-text remaining-text)
        (enable-extensions (uiop:ensure-list enable-extensions)))
    `(deftest ,name ()
       ,@ (when known-failure
            '((skip)))
       (let ((expected ,expected)
             (catched-condition nil))
         (progv
             ',enable-extensions
             (mapcar (constantly t)
                     ',enable-extensions)
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
                    (let ((remaining-text
                            (subseq ,text remaining-text-start)))
                      (is (string= remaining-text
                                   ,expected-remaining-text)))
                    (is (not remaining-text-start)))
                (when ,parse-should-succeed
                  (is parse-succeeded)))
               (,fail-expected
                (is (typep catched-condition
                           ,fail-expected)))
               (t
                (is (not parse-succeeded))))))))))
