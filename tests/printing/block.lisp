(in-package 3bmd-tests)


(def-print-test print-blockquote-md-1
  :format :markdown
  :text "> 1 2 3
> 4 5 6
"
  :expected "> 1 2 3
> 4 5 6
")

(def-print-test print-verbatim-md-1
  :format :markdown
  :text "    1 2 3
    4 5 6
"
  :expected "    1 2 3
    4 5 6
")

(def-print-test print-blockquote-md-2
  :format :markdown
  :text "> 1 2 3
4 5 6
"
  :expected "> 1 2 3
> 4 5 6
")

(def-print-test print-nested-blockquote-1
  :format :markdown
  :text "- 1
    > 2 3
    > - 4
    >     > 5 6
    >     > 7 8
    > 9
 "
  :expected "- 1

    > 2 3
    > - 4
    >     > 5 6
    >     > 7 8
    > 9
")

(def-print-test print-nested-heading-1
  :format :markdown
  :text "- 1
    # h
    p"
  :expected "- 1

    # h

    p
")



(def-print-test print-verbatim-1
  :format :markdown
  :text "    Foo
    Bar

Just a paragraph"
  :expected  "    Foo
    Bar

Just a paragraph")

(def-print-test print-markdown-misc
  :format :markdown
  :text "\\*notemph\\* \\_notemph2\\_ \\ \\`notcode\\`
"
  :expected "\\*notemph\\* \\_notemph2\\_ \\ \\`notcode\\`")

(def-print-test print-markdown-misc-2
  :format :markdown
  :text "[id1]: http://some.link.com/ \"with some title\""
  ;; not sure about the extra spacing?
  :expected "
[id1]: http://some.link.com/ \"with some title\"
")

(def-print-test print-markdown-doc
  :format :markdown
  :text     "### heading

- outer item

    dsfdsf *emph* **strong** ***strongemph***
    kjdsf [asdf](#xxx \"*title*\") <http://quotenil.com> <mega@retes.hu>

    - b1

        inside `````co````de`````

            codeblock 234
            sdfkj kjsdf

    - b2

    dsfdsf
    llll

    1. xxx

    2. yyy

    ---

    para [ref-link][sfd] ![image](#dsfa \"img\") ![img][32]
    sddsf <a href=\"#xxx\">ddd</a> <!-- comment -->

em-dash -- en-dash --- lrsa <-> <- -> <=> <= => ... foo's

[id1]: http://some.link.com/ \"with some title\"

plain
"
  :expected     "### heading

- outer item

    dsfdsf *emph* **strong** ***strongemph***
    kjdsf [asdf](#xxx \"*title*\") <http://quotenil.com> <mega@retes.hu>

    - b1

        inside `````co````de`````

            codeblock 234
            sdfkj kjsdf

    - b2

    dsfdsf
    llll

    1. xxx

    2. yyy

    ---

    para [ref-link][sfd] ![image](#dsfa \"img\") ![img][32]
    sddsf <a href=\"#xxx\">ddd</a> <!-- comment -->

em-dash -- en-dash --- lrsa \\<-> \\<- -> \\<=> \\<= => ... foo's

[id1]: http://some.link.com/ \"with some title\"

plain"
)

(def-print-test print-escaped-brackets
  :format :markdown
  :text     "[\\[x\\]](#y)"
  :expected "[\\[x\\]](#y)")
