(defsystem 3bmd-ext-definition-lists
  :description "extension to 3bmd implementing 'PHP Markdown Extra' style definition lists"
  :depends-on (3bmd colorize alexandria)
  :serial t
  :components ((:file "definition-lists")))
