Common Lisp [Markdown][] -> html converter, using [esrap][] for parsing, and grammar based on [peg-markdown][].

Currently a bit slow and uses lots of RAM for large documents (particularly when using the top-level `doc` parser instead of reading documents as a sequence of `block`s), but seems to handle the tests from [peg-markdown] reasonably well.

Note that this library processes [Markdown][] and not the newer (and better specified) [CommonMark][], so may not behave quite as expected for people used to the latter. See [issue #53](https://github.com/3b/3bmd/issues/53) for some discussion on why CommonMark support isn't currently planned, and would probably be in a separate library if it were implemented.

todo:

* clean up API
* figure out how to automate testing (closure-html + `tree-equal`? need some way to normalize whitespace though), and add tests
* optimize grammar
* optimize esrap

[markdown]: http://daringfireball.net/projects/markdown/
[esrap]: https://github.com/scymtym/esrap
[peg-markdown]: https://github.com/jgm/peg-markdown
[peg/leg]: http://piumarta.com/software/peg/peg.1.html
[CommonMark]: https://commonmark.org/

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

* Loading `3bmd-ext-wiki-links.asd` adds support for parsing simple `[[]]` style wiki links:
     If `3bmd-wiki:*wiki-links*` is non-`NIL` while parsing, wiki links of the form `[[foo]]` or `[[foo|...]]` will be parsed, where `...` is one or more optional args separated by `|` characters.
    By default, wiki links will just print the `foo` part as normal text. To integrate into an actual wiki, users should bind `3bmd-wiki:*wiki-processor*` during printing, and define a method on `3bmd-wiki:process-wiki-link` that specializes on the value of `3bmd-wiki:*wiki-processor*` to create an HTML link from the `foo` and arguments. (API subject to change.)


* Loading `3bmd-ext-code-blocks.asd` adds support for github style fenced code blocks, with `colorize` support:
      If `3bmd-code-blocks:*code-blocks*` is non-`NIL` while parsing, in addition to normal indented verbatim blocks, ```` ``` ```` can be used to delimit blocks of code:

        ```
        This block doesn't specify a language for colorization
        ```
    or

        ```lisp
        ;;; this block will be colorized as Common Lisp
        (defun foo (bar)
          (list bar))
        ```

    Language names ignore case and whitespace, so `Common Lisp` and `commonlisp` are treated the same, see `3bmd-code-blocks:*colorize-name-map*` for full list of supported language names, or add names to that to recognize a custom colorize `coloring-type`.
    If a language name is not specified after the opening ```` ``` ````, `3bmd-code-blocks:*code-blocks-default-colorize*` can be set to one of the keywords naming a `coloring-type` recognized by `colorize` to specify a default, otherwise the block will not be colorized.

    Can optionally use `Pygments` instead of `colorize` by setting `3bmd-code-blocks:*renderer*` to  `:pygments`. Lexer and formatter options (`-O`) can be specified like ```` ```c++|linenos=1````.

    Some attempt has been made to avoid interpretation of the options by the shell when calling `pygmentize`, but you should probably audit the code and test the interaction with the implementation of `uiop:run-program` on your implementation of choice before using it on untrusted input. Pygments html formatter creates arbitrary files when passed `-Ofull,cssfile=filename`, so parameters with the substring `cssfile` are ignored (`noclobber_cssfile=True` is also set by default, but that only prevents overwriting, not creation). Users with untrusted input may want to audit that as well to make sure there are no other dangerous options or ways to get around the exact substring check.


    Can optionally use [`Chroma`](https://github.com/alecthomas/chroma) instead of `colorize` or `Pygments` by setting `3bmd-code-blocks:*renderer*` to  `:chroma`. Change the embedded theme of the `:chroma` code block via `3bmd-code-blocks:*chroma-style*`. The various styles for Chroma can be viewed via `chroma --list`.

    If no highlighting is desired, in case of using a JavaScript highlighter, it is possible to specify `:nohighlight` as `3bmd-code-blocks:*renderer*`.
    In this case the `pre` tags `class` attribute is rendered with the defined language. So:
    
    ```
    ```lisp
    (defun foo ())
    ```
    
    Is rendered as: 
    
    ```
    <pre class="lisp"><code>...</code></pre>
    ```
    
    To change the format used for rendering the `class` attribute value you can set a different format to `*code-blocks-pre-class-format*` which defaults to `~a` in order to render the language as parsed from the triple ticks block. I.e.: setting the format `(setf 3bmd-code-blocks:*code-blocks-pre-class-format* "brush: ~a;")` will render:
    
    ```
    <pre class="brush: lisp;"><code>...</code></pre>    
    ```

    Inline code spans (like `` `this` ``) can also be optionally highlighted with `3bmd-ext-code-blocks`. Set `3bmd-code-blocks:*render-code-spans*` to true, and set `3bmd-code-blocks:*render-code-spans-lang*` to the desired language.

* Loading `3bmd-ext-definition-lists.asd` adds support for parsing PHP Markdown Extra style definition lists:
     If `3bmd-definition-lists:*definition-lists*` is non-`NIL` while parsing, the following definition list will be recognized (see <http://michelf.ca/projects/php-markdown/extra/#def-list>):

        Term
        : definition

* Loading `3bmd-ext-tables.asd` adds support for parsing PHP Markdown Extra style tables:
     If `3bmd-tables:*tables*` is non-`NIL` while parsing, the following will be recognized as tables (see <http://michelf.ca/projects/php-markdown/extra/#table>):

        | Content Cell  | Content Cell  |
        | Content Cell  | Content Cell  |

        | First Header  | Second Header |
        | ------------- | ------------- |
        | Content Cell  | Content Cell  |
        | Content Cell  | Content Cell  |

        | Name | Description          |
        | ------------- | ----------- |
        | Help      | Display the help window.|
        | Close     | Closes a window     |

        | Left-Aligned  | Center Aligned  | Right Aligned |
        | :------------ |:---------------:| -----:|
        | col 3 is      | some wordy text | $1600 |
        | col 2 is      | centered        |   $12 |
        | zebra stripes | are neat        |    $1 |

    The following simplified table style is not supported, because it is ambiguous,
especially, without heading:

    ```
    First Header  | Second Header
    ------------- | -------------
    Content Cell  | Content Cell
    Content Cell  | Content Cell
    ```

* Loading `3bmd-youtube.asd` adds support for embedding youtube videos. If `3bmd-youtube:*youtube-embeds*` is non-`NIL` while parsing, the shorthand syntax `!yt[video-id(|options)]` can be used. For example:

        !yt[nbY-meOL57I]
        !yt[nbY-meOL57I|width=20,allowfullscreen]"

* Loading `3bmd-ext-math.asd` adds support for math markup with libraries like MathJax. If `3bmd-math:*math*` is non-`NIL` while parsing, the shorthand syntax `$$ latex markup $$` can be used. For example:

        $$
        \frac{\partial E}{\partial y} = \frac{\partial }{\partial y} \frac{1}{n}\sum_{i=1}^{n} (y_i - a_i)^2
        $$
