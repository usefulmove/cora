;;; cora-test.el --- Unit tests for Cora programming language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 30, 2023
;; Modified: September 2, 2023
;; Version: 0.2.1
;; Keywords: language extensions internal lisp tools emacs
;; Homepage: https://github.com/usefulmove/cora
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Cora language unit tests
;;
;; Code:

(load-file "~/repos/cora/src/cora.el") ; load Cora language



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun cora-test-compound (error-prelude)
  (when (not (zero? (- 204 (sum (map
                                  (lambda (a) (* a a))
                                  (range (inc 8)))))))
    (error (concat error-prelude "error: compound test(s) failed"))))


(defun cora-test-compound2 (error-prelude)
  (when (not (equal? 1157625
                     (prod (filter 'odd? (map
                                           (lambda (a) (* a a a))
                                           (range (dec 10)))))))
    (error (concat error-prelude "error: compound2 test(s) failed"))))


(defun cora-test-compound3 (error-prelude)
  (when (any? 'even? '(3 1 5 9 7))
    (error (concat error-prelude "error: compound3 test(s) failed"))))


(defun cora-test-function-composition (error-prelude)
  (when (not (equal? (thread 5
                       'sqrt
                       (lambda (a) (- a 1))
                       (lambda (a) (/ a 2)))
                      (call (pipe 'sqrt
                                    (lambda (a) (- a 1))
                                    (lambda (a) (/ a 2)))
                       5)))
    (error (concat error-prelude "error: function composition (1) test(s) failed"))))


(defun cora-test-function-composition2 (error-prelude)
  (when (not (equal? (thread 5
                       'sqrt
                       (lambda (a) (- a 1))
                       (lambda (a) (/ a 2)))
                     (call (compose (lambda (a) (/ a 2))
                                    (lambda (a) (- a 1))
                                    'sqrt)
                       5)))
    (error (concat error-prelude "error: function composition (2) test(s) failed"))))


; TODO to be defined
;(defun cora-test-currying (error-prelude)
;  )


; TODO missing test coverage
; fold partial curry2 assert-equal
; all? init join-chars gcd end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run unit tests

(defun test-run-tests (&rest tests)
  (letrec ((prelude "cora-test ... ")
           (execute-tests (lambda (fns)
                            (cond ((null fns) nil)
                                  (t (call (car fns) prelude)
                                     (call execute-tests (cdr fns)))))))
    (message (concat prelude "running tests..."))
    (call execute-tests tests)
    (message (concat prelude "passed all tests"))))

(test-run-tests
  'cora-test-compound
  'cora-test-compound2
  'cora-test-compound3
  'cora-test-function-composition
  'cora-test-function-composition2)
  ;'cora-test-currying)





(provide 'cora-test)
;;; cora-test.el ends here
