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
  :warn "Unresolvable reference link (:LABEL (\"something\") :DEFINITION \"non-existent\")
")

