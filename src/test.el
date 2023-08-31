;;; test.el --- Unit tests for epic functional programming algorithms library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 30, 2023
;; Modified: August 30, 2023
;; Version: 0.0.1
;; Keywords: extensions internal lisp tools
;; Homepage: https://github.com/usefulmove/epic
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: epic library unit tests
;;
;; Code:

(load-file "~/repos/epic/src/epic.el") ; load epic functional library


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun test-epic-placeholder (error-prelude)
  (when (not (equal 1 0))
    (error (concat error-prelude "error: encrypt char test(s) failed"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test execution

(defun test-run-tests ()
  (let* ((prelude "epic-test ... "))
    (message (concat prelude "running tests..."))
    (test-epic-placeholder prelude) ; placeholder
    (message (concat prelude "passed all tests"))))

(test-run-tests)




(provide 'test)
;;; test.el ends here
