(fiasco:define-test-package #:3bmd-tests
  (:export #:def-grammar-test
           #:def-print-test))

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
                                   known-failure
                                   bind)
  (let ((expected-remaining-text remaining-text)
        (enable-extensions (uiop:ensure-list enable-extensions)))
    `(deftest ,name ()
       ,@ (when known-failure
            '((skip)))
       (let ((expected ,expected)
             (catched-condition nil))
         (progv
             ',(append enable-extensions
                       (mapcar 'first bind))
             ',(append (mapcar (constantly t)
                               enable-extensions)
                       (mapcar 'second bind))
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

;; todo: this should do something smarter than exact string match for
;; html compare, but won't worry about it until there are enough tests
;; and code changes for manual verification to be too much work.
(defmacro def-print-test (name &key
                                 (rule '3bmd-grammar::doc)
                                 text
                                 (format :html)
                                 expected
                                 warn
                                 (enable-extensions nil)
                                 known-failure
                                 bind)
  (let ((enable-extensions (uiop:ensure-list enable-extensions)))
    `(deftest ,name ()
       ,@(when known-failure
           '((skip)))
       (let ((expected ,expected)
             (signalled-warnings nil))
         (progv
             ',(append enable-extensions
                       (mapcar 'first bind))
             ',(append (mapcar (constantly t)
                               enable-extensions)
                       (mapcar 'second bind))
           (multiple-value-bind (parsed remaining-text parse-succeeded)
               (esrap:parse ',rule ,text)
             (is parse-succeeded)
             (is (not remaining-text))
             (let ((printed
                     (handler-bind ((warning (lambda (c)
                                               (push c signalled-warnings)
                                               (muffle-warning c))))
                       (with-output-to-string (s)
                         (3bmd:print-doc-to-stream parsed s :format ,format)))))
               (is (equalp printed expected))
               ,@(if warn
                     ;; just supporting 1 warning for now
                     `((is (= 1 (length signalled-warnings)))
                       (let ((signalled-warning (format nil "~a"
                                                        (car signalled-warnings))))
                         (is (string= signalled-warning ,warn))))

                     `((is (not signalled-warnings))))
               ,@(when (eql format :markdown)
                   ;; if printing to markdown, we should be able to
                   ;; parse it and get the same parse as original input
                   ;; (printed might not be same as original input, but
                   ;; should have same meaning)
                   `((multiple-value-bind (reparsed
                                           remaining-text-2 parse-succeeded-2)
                         (esrap:parse ',rule ,text)
                       (is parse-succeeded-2)
                       (is (not remaining-text-2))
                       (is (equalp parsed reparsed))))))))))))
