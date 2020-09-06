(defsystem 3bmd-ext-code-blocks
  :description "extension to 3bmd implementing github style ``` delimited code blocks, with support for syntax highlighting using colorize, pygments, or chroma"
  :depends-on (3bmd colorize alexandria split-sequence #-asdf3 :uiop)
  :serial t
  :components ((:file "code-blocks")
               (:static-file "pygmentize.py")))
