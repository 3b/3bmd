(in-package 3bmd-tests)

(def-grammar-test raw-html-test-1
  :rule 3bmd-grammar::inlines
  :text "<!-- Any HTML comment can be inlined -->"
  :expected '((:RAW-HTML "<!-- Any HTML comment can be inlined -->")))


(def-grammar-test raw-html-test-2
  :rule 3bmd-grammar::inlines
  :text "<?xml-stylesheet href=\"http://www.w3.org/StyleSheets/TR/W3C-REC.css\" type=\"text/css\"?>"
  :expected '((:RAW-HTML "<?xml-stylesheet href=\"http://www.w3.org/StyleSheets/TR/W3C-REC.css\" type=\"text/css\"?>")))

(def-grammar-test raw-html-test-3
  :rule 3bmd-grammar::inlines
  :text "<br/>"
  :expected '((:RAW-HTML "<br/>")))

(def-grammar-test raw-html-test-4
  :rule 3bmd-grammar::inlines
  :text "<div id=\"some-block\">Hello World!</div>"
  ;; For some reason, 3BMD parses html tags (even which renders as blocks)
  ;; as a sequence of separate entities
  :expected '((:RAW-HTML "<div id=\"some-block\">")
              "Hello"
              " "
              "World"
              "!"
              (:RAW-HTML "</div>")))

(def-grammar-test raw-html-test-5
  :rule 3bmd-grammar::inlines
  ;; A sequense of such inlines might span a multiple lines
  :text "<div id=\"some-block\">
Hello World!
</div>"
  :expected '((:RAW-HTML "<div id=\"some-block\">")
              "
"
              "Hello"
              " "
              "World"
              "!"
              "
"
              (:RAW-HTML "</div>")))


(def-grammar-test raw-html-test-6
  :rule 3bmd-grammar::inlines
  ;; And even a single inline HTML tag might span multiple lines:
  :text "
<div id=\"some-block\"
     class=\"menu-item\">"
  :expected '("
"
              (:RAW-HTML "<div id=\"some-block\"
     class=\"menu-item\">")))


(def-grammar-test raw-html-nested-in-a-list
  :rule 3bmd-grammar::doc
  :text "* List item

  <div id=\"some-block\"
       class=\"menu-item\">
    Content
  </div>
"
  
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "List" " " "item")))
              ;; TODO:
              ;; This RAW-HTML should be a part of the previos LIST-ITEM
              ;; and probably to be included in the PARAGRAPH instead of the PLAIN
              (:PLAIN "  "
               (:RAW-HTML "<div id=\"some-block\"
       class=\"menu-item\">")
               "
"
               ;; This indentation should be removed:
               "    "
               "Content"
               "
"
               ;; This indentation should be removed:
               "  "
               (:RAW-HTML "</div>"))))


