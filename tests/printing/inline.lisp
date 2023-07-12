(in-package 3bmd-tests)

(def-print-test print-reference-link
  :text "[foo][bar]

[bar]: /url \"title\""
  :expected "<p><a href=\"/url\" title=\"title\">foo</a></p>
")

(def-print-test print-missing-reference-link ;; bug # 47
  :text "[something][non-existent]"
  :expected "[something][non-existent]
"
  :warn "Unresolvable reference link (:LABEL (\"something\") :DEFINITION (\"non-existent\"))
")

(def-print-test print-missing-reference-link-2
  :text "[non-existent]"
  :expected "[non-existent]
"
  :warn "Unresolvable reference link (:LABEL (\"non-existent\") :TAIL NIL)
")

(def-print-test print-missing-reference-link-3
  :text "[non-existent][]"
  :expected "[non-existent][]
"
  :warn "Unresolvable reference link (:LABEL (\"non-existent\") :TAIL \"[]\")
")

(def-print-test print-missing-reference-link-4
  :text "[*non-existent*]"
  :expected "[<em>non-existent</em>]
"
  :warn "Unresolvable reference link (:LABEL ((:EMPH \"non-existent\")) :TAIL NIL)
")

(def-print-test print-missing-reference-link-5
  :text "[*non-existent*][]"
  :expected "[<em>non-existent</em>][]
"
  :warn "Unresolvable reference link (:LABEL ((:EMPH \"non-existent\")) :TAIL \"[]\")
")

(def-print-test print-missing-reference-link-6
  :text "[a][*non-existent*]"
  :expected "[a][<em>non-existent</em>]
"
  :warn "Unresolvable reference link (:LABEL (\"a\") :DEFINITION ((:EMPH \"non-existent\")))
")

(def-print-test print-formatted-reference-link-1
  :text "[*l*][*b*]

[*b*]: foo"
  :expected "<p><a href=\"foo\" ><em>l</em></a></p>
")

(def-print-test print-formatted-reference-link-2
  :text "[*l*][`b`]

[`b`]: foo"
  :expected "<p><a href=\"foo\" ><em>l</em></a></p>
")

(def-print-test print-formatted-reference-link-3
  :format :markdown
  :text "[*l*][`b`]
[`b`][]
[*a*][*b*]
[*c*][]

[`b`]: foo"
  :expected "[*l*][`b`]
[`b`][]
[*a*][*b*]
[*c*][]

[`b`]: foo
")

(def-print-test highlight-code-span-1
  :enable-extensions 3bmd-code-blocks:*code-blocks*
  :bind ((3bmd-code-blocks:*render-code-spans* t)
         (3bmd-code-blocks:*render-code-spans-lang* :common-lisp))
  :rule 3bmd-grammar::doc
  :text "`;abc`"
  :expected '"<code><span class=\"code\"><span class=\"comment\">;abc</span></span></code>
")

(def-print-test print-reference-link-test-1
  :text "[link]

[link]: http://example.com/ \"title\""
  :expected '"<p><a href=\"http://example.com/\" title=\"title\">link</a></p>
")

(def-print-test print-reference-link-test-2
  :text "
### [link]

[link]: http://example.com/ \"title\""
  :expected '"<h3><a href=\"http://example.com/\" title=\"title\">link</a></h3>
")

(def-print-test print-heading-1
  :text "# heading
"
  :expected "<h1>heading</h1>
")

(def-print-test print-heading-2
  :text "\\# heading
"
  :expected "# heading
")

(def-print-test print-heading-3
  :text " # heading
"
  :expected " # heading
")

(def-print-test print-heading-4
  :text " \\# heading
"
  :expected " # heading
")

(def-print-test print-heading-5
  :text "`# heading`
"
  :expected "<code># heading</code>
")

(def-print-test print-heading-6
  :text "`\\# heading`
"
  :expected "<code>\\# heading</code>
")

(def-print-test print-heading-7
  :text "    # heading
"
  :expected "<pre><code># heading
</code></pre>
")

(def-print-test print-heading-8
  :text "    \\# heading
"
  :expected "<pre><code>\\# heading
</code></pre>
")

(def-print-test print-curly-brackets
  :text "\\inlinelatexormathjax{arg}
"
  :expected "\\inlinelatexormathjax{arg}
")
