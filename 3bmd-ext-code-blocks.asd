(defsystem 3bmd-ext-code-blocks
  :description "extension to 3bmd implementing github style ``` delimited code blocks, with support for syntax highlighting using colorize or pygments"
  :depends-on (3bmd colorize alexandria #-asdf3 :uiop)
  :serial t
  :components ((:file "code-blocks")))
