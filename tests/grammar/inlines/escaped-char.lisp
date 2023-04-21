(in-package 3bmd-tests)


(def-grammar-test escaped-chars-test-1
  :rule 3bmd-grammar::inlines
  :text "\\-\\\\\\`\\|\\*\\_\\{\\}\\[\\]\\(\\)\\#\\+\\.\\!\\<\\>"
  :expected '("-" "\\" "`" "|" "*" "_"
              "{" "}" "[" "]" "(" ")" "#"
              "+" "." "!" "<" ">"))
