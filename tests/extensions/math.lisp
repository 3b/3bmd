(fiasco:define-test-package #:3bmd-ext-math-tests
  (:import-from #:3bmd-tests #:def-grammar-test #:def-print-test)
  (:use #:3bmd-math))

(in-package #:3bmd-ext-math-tests)

(def-grammar-test math-inline-1
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "$x_0$"
  :expected '(:plain (:math-inline-1 "x_0")))

(def-print-test print-math-inline-1
  :enable-extensions 3bmd-math:*math*
  :format :markdown
  :text "$x_0$"
  :expected "$x_0$")

(def-grammar-test math-inline-1/space-after-open
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "$ x_0$"
  :expected '(:plain "$" " " "x_0" "$"))

(def-print-test print-math-inline-1/space-after-open
  :enable-extensions 3bmd-math:*math*
  :format :markdown
  :text "$ x_0$"
  :expected "\\$ x\\_0\\$")

(def-grammar-test math-inline-1/space-before-close
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "$x_0 $"
  :expected '(:plain "$" "x_0" " " "$"))

(def-print-test print-math-inline-1/space-before-close
  :enable-extensions 3bmd-math:*math*
  :format :markdown
  :text "$x_0 $"
  :expected "\\$x\\_0 \\$")

(def-grammar-test math-inline-1/escaped
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "\\$x_0$"
  :expected '(:plain "$" "x_0" "$"))

(def-grammar-test math-inline-1/both-escaped
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "\\$x_0\\$"
  :expected '(:plain "$" "x_0" "$"))

(def-grammar-test math-inline-2
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "$`x_0`$"
  :expected '(:plain (:math-inline-2 "x_0")))

(def-print-test print-math-inline-2
  :enable-extensions 3bmd-math:*math*
  :format :markdown
  :text "$`x_0`$"
  :expected "$`x_0`$")

(def-grammar-test math-inline-2/escaped
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "\\$`x_0`$"
  :expected '(:plain "$" (:code "x_0") "$"))

(def-print-test print-math-inline-2/escaped
  :enable-extensions 3bmd-math:*math*
  :format :markdown
  :text "\\$`x_0`$"
  :expected "\\$`x_0`\\$")

(def-grammar-test math-inline-2/both-escaped
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "\\$`x_0`\\$"
  :expected '(:plain "$" (:code "x_0") "$"))

(def-print-test print-math-inline-2/both-escaped
  :enable-extensions 3bmd-math:*math*
  :format :markdown
  :text "\\$`x_0`\\$"
  :expected "\\$`x_0`\\$")

(def-grammar-test math-inline-3
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "$$x_0$$ x"
  :expected '(:plain (:math-inline-3 "x_0") " " "x"))

(def-print-test print-math-inline-3
  :enable-extensions 3bmd-math:*math*
  :format :markdown
  :text "$$x_0$$ x"
  :expected "$$x_0$$ x")

(def-grammar-test math-inline-3/one-escaped
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "\\$$x_0$$ x"
  :expected '(:plain "$" (:math-inline-1 "x_0") "$" " " "x"))

(def-print-test print-math-inline-3/one-escaped
  :enable-extensions 3bmd-math:*math*
  :format :markdown
  :text "\\$$x_0$$"
  :expected "\\$$x_0$\\$")

(def-grammar-test math-inline-3/two-escaped
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "\\$\\$x_0$$ x"
  :expected '(:plain "$" "$" "x_0" "$" "$" " " "x"))

(def-print-test print-math-inline-3/two-escaped
  :enable-extensions *math*
  :format :markdown
  :text "\\$\\$x_0$$ x"
  :expected "\\$\\$x\\_0\\$\\$ x")

(def-grammar-test math-block
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "$$x_0$$"
  :expected '(:math-block "x_0"))

(def-print-test print-math-block
  :enable-extensions *math*
  :format :markdown
  :text "$$x_0$$"
  :expected "$$x_0$$
")

(def-grammar-test math-block/trailing-whitespace
  :enable-extensions *math*
  :rule 3bmd-grammar::%block
  :text "$$x_0$$ "
  :expected '(:math-block "x_0"))

(def-print-test print-math-block/trailing-whitespace
  :enable-extensions *math*
  :format :markdown
  :text "$$x_0$$ "
  :expected "$$x_0$$
")
