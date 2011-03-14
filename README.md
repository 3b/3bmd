Common Lisp [Markdown][] -> html converter, using [esrap][] for parsing, and grammar based on [peg-markdown][].

Currently a bit slow and uses lots of RAM for large documents (particularly when using the top-level `doc` parser instead of reading documents as a sequence of `block`s), but seems to handle the tests from [peg-markdown] reasonably well.

todo:

* clean up API
* obfuscate `mailto:` links
* figure out how to automate testing (closure-html + `tree-equal`? need some way to normalize whitespace though), and add tests
* optimize grammar
* optimize esrap

[markdown]: http://daringfireball.net/projects/markdown/
[esrap]: https://github.com/nikodemus/esrap
[peg-markdown]: https://github.com/jgm/peg-markdown
[peg/leg]: http://piumarta.com/software/peg/peg.1.html

## Extensions:

* If `3bmd:*smart-quotes*` is non-`NIL` while parsing, some extra patterns will be recognized and converted as follows (outside code blocks):
    * `'`single quoted strings`'` -> `&lsquo;` ... `&rsquo;` like &lsquo;single quoted string&rsquo;
      (with slightly ugly heuristics to avoid contractions)
    * other single quotes `'` -> `&apos;` &apos;
    * `"`double quoted strings`"` -> `&ldquo;` ... `&rdquo;`, like &ldquo;double quoted string&rdquo;
    * ellipsis `...` or `. . .` -> `&hellip;`, &hellip;
    * en dash `--` -> `&ndash;`, &ndash;
    * em dash `---` -> `&mdash;`, &mdash;
    * left right arrow `<->` -> `&harr;`, &harr;
    * left arrow `<-` -> `&larr;`, &larr;
    * right arrow `->` -> `&rarr;`, &rarr;
    * left right double arrow `<=>` -> `&hArr`, &hArr;
    * left double arrow `<=` -> `&lArr;`, &lArr;
    * right double arrow `=>` -> `&rArr;`, &rArr;

* If `3bmd:*wiki-links*` is non-`NIL` while parsing, wiki links of the form `[[foo]]` or `[[foo|...]]` will be parsed, where `...` is one or more optional args separated by `|` characters.  
    By default, wiki links will just print the `foo` part as normal text. To integrate into an actual wiki, users should bind `3bmd:*wiki-processor*` during printing, and define a method on `3bmd:process-wiki-link` that specializes on the value of `3bmd:*wiki-processor*` to create an HTML link from the `foo` and arguments. (API subject to change.)
