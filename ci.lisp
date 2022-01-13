(uiop:define-package #:3bmd-ci
  (:use #:cl))
(in-package #:3bmd-ci)

;; To update workflows in the .github/ subdirectory,
;; do (ql:quickload :3bmd-ci)
;; 
;; More information about workflow builder:
;; https://40ants.com/ci/

(40ants-ci/workflow:defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :asdf-system "3bmd-tests")))
