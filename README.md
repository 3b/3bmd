Common Lisp [Markdown][] -> html converter, using [esrap][] for parsing, and grammar based on [peg-markdown][].

Currently a bit slow and uses lots of RAM for large documents (particularly when using the top-level `doc` parser instead of reading documents as a sequence of `block`s), but seems to handle the tests from [peg-markdown] reasonably well.

todo:

* clean up API
* obfuscate `mailto:` links
* figure out how to automate testing (closure-html + `tree-equal`? need some way to normalize whitespace though), and add tests
* figure out how to do optional rules (like `&{...}` from [peg/leg][]), and add common extensions
* optimize grammar
* optimize esrap

[markdown]: http://daringfireball.net/projects/markdown/
[esrap]: https://github.com/nikodemus/esrap
[peg-markdown]: https://github.com/jgm/peg-markdown
[peg/leg]: http://piumarta.com/software/peg/peg.1.html
