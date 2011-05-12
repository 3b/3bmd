(in-package #:3bmd-grammar)

;;; to extend gammar, we have a few specific extension points:
;;; new block types
;;;   top level blocks, like paragraphs, tables, quotes, code blocks, etc
;;; new inline types
;;;   stuff valid within a block, like special characters, links, etc
;;; new special characters
;;;   some rules need to recognize special characters of other rules,
;;;   so add them here
;;; new escaped characters
;;;   extra characters that \ should escape


(defun add-expression-to-list (new list &key before after (test 'eql))
  (if (find new list :test test)
      list
      (let* ((len (length list))
             (min (loop for i in after
                     when (eql i :end)
                     return len
                     ;; todo: add some way to check for symbols in
                     ;; packages that might not exist (so extensions
                     ;; that know about eachother can make sure their
                     ;; rules are in correct order if needed)
                     else maximize (or (position i list) 0)))
             (max (if before
                      (loop for i in before
                         when (eql i :start)
                         return 0
                         minimize (or (position i list) len))
                      len)))
        (when (< max min)
          (error "can't add rule to expression, conflicting before=~s(~s) after=~s(\
~s)" before max after min))
        (if (zerop min)
            (cons new list)
            (let ((list (copy-list list)))
              (push new (cdr (nthcdr (1- min) list)))
              list)))))

(defun %make-definer (extension-flag name expression options var rule exp)
  (let ((characters (cdr (assoc :character-rule options)))
        (escapes (cdr (assoc :escape-char-rule options)))
        (after (cdr (assoc :after options)))
        (before (cdr (assoc :before options))))
    `(progn
       ;; define the flag to make the trivial case easier
       (defvar ,extension-flag nil)
       ;; define a rule for extension chars if any
       ,@ (when characters
            `((defrule ,(first characters)
                  (or ,@(rest characters))
                (:when ,extension-flag))
              (setf %extended-special-char-rules%
                    (add-expression-to-list ',(first characters)
                                            %extended-special-char-rules%))
              (esrap:change-rule 'extended-special-char
                                            (cons 'or %extended-special-char-rules%))))
          ;; define a rule for escaped chars if any
       ,@ (when escapes
            `((defrule ,(first escapes)
                  (or ,@(rest escapes))
                (:when ,extension-flag))
              (setf %extended-escape-char-rules%
                    (add-expression-to-list ',(first escapes)
                                            (cons 'or %extended-escape-char-rules%)))
              (esrap:change-rule
               'extended-escape-character
               %extended-escape-char-rules%)))
          ;; define extension rule, passing any left-over args to esrap
       (defrule ,name ,expression
         (:when ,extension-flag)
         ,@(remove-if (lambda (a)
                        (member (car a) '(:character-rule
                                          :escape-char-rule
                                          :after :before)))
                      options))
       (setf ,var
             (add-expression-to-list ',name
                                     ,var
                                     ,@(when before `(:before ',before))
                                     ,@(when after `(:after ',after)) ))
       (esrap:change-rule ',rule ,exp))))

(defmacro define-extension-inline (extension-flag name expression &body options)
  (%make-definer extension-flag name expression options '%inline-rules% 'inline
                 '(cons 'or %inline-rules%)))

(defmacro define-extension-block (extension-flag name expression &body options)
  (%make-definer extension-flag name expression options '%block-rules% 'block
                 '`(and (* blank-line) (or ,@%block-rules%))))

