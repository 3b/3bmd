(in-package 3bmd-tests)


(def-grammar-test parse-html-block-1
  :rule 3bmd-grammar::html-block
  :text "<div id=\"100500\" class=\"foo\">
  Bar
</div>
"
  :expected '(:HTML "<div id=\"100500\" class=\"foo\">
  Bar
</div>"))


(def-grammar-test parse-html-block-inside-a-list
  :text "* Foo

  <div id=\"100500\" class=\"foo\">
    Bar
  </div>

* Bar
"
  ;; TODO: This example result is completely wrong:
  :expected '((:BULLET-LIST (:LIST-ITEM
                             (:PLAIN "Foo")))
              (:PARAGRAPH
               (:RAW-HTML "<div id=\"100500\" class=\"foo\">")
               "
"
               "    "
               "Bar"
               "
"
               "  "
               (:RAW-HTML "</div>"))
              (:BULLET-LIST (:LIST-ITEM
                             (:PLAIN "Bar")))))

(def-grammar-test parse-html-processing-instruction-1 ;; bug 37
  :text "inline <?this is a valid processing instruction?>
"
  :expected '((:plain "inline" " "
               (:raw-html "<?this is a valid processing instruction?>"))))

(def-grammar-test parse-html-processing-instruction-2 ;; bug 37
  :text "block

<?this is a valid processing instruction?>
"
  :expected '((:PARAGRAPH "block")
              (:html "<?this is a valid processing instruction?>")))

(def-grammar-test parse-html-processing-instruction-3 ;; bug 37
  :text "block

<?this is a valid 
 processing instruction?>
"
  :expected '((:PARAGRAPH "block")
              (:HTML "<?this is a valid 
 processing instruction?>")))
