(defsystem 3bmd-ci
  :description "Load this system to rebuild GitHub Actions workflows"
  ;; 40ANTS-CI is not in Quicklisp yet. See
  ;; https://github.com/quicklisp/quicklisp-projects/issues/2121
  ;; but it can be installed from Ultralisp.org
  :depends-on (40ants-ci)
  :serial t
  :components ((:file "ci")))
