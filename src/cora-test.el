;;; cora-test.el --- Unit tests for Cora functional programming -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 30, 2023
;; Modified: September 8, 2023
;; Version: 0.2.11
;; Keywords: language extensions internal lisp tools emacs
;; Homepage: https://github.com/usefulmove/cora
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary: Cora source code ~/repos/cora/src/cora.el
;;
;;  Description: Cora unit tests
;;
;; Code:

(load-file "~/repos/cora/src/cora.el") ; load Cora language



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun cora-test-compound (error-prelude)
  (when (not (zero? (- 204 (sum (map
                                  (fn (a) (* a a))
                                  (range (inc 8)))))))
    (error (concat error-prelude "error: compound test(s) failed"))))


(defun cora-test-compound2 (error-prelude)
  (assert-equal
    1157625
    (prod (filter 'odd? (map
                          (fn (a) (* a a a))
                          (range (dec 10)))))
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    '(3 1)
    (init '(3 1 2))
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    2
    (end '(3 1 2))
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    t
    (all? 'even? (map
                   (fn (a) (* 2 a))
                   (range (inc 31))))
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    6
    (gcd 18 30 12)
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (not (any? 'ascii-numeric? (list 46 47 58 59)))
    (all? 'ascii-numeric? (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    '(3 1 2 1 2)
    (flatten '(3 1 (2 1 2)))
    (concat error-prelude "error: compound2 test(s) failed")))


(defun cora-test-compound3 (error-prelude)
  (when (any? 'even? '(3 1 5 9 7))
    (error (concat error-prelude "error: compound3 test(s) failed"))))


(defun cora-test-function-composition (error-prelude)
  (when (not (equal? (thread 5
                       'sqrt
                       (fn (a) (- a 1))
                       (fn (a) (/ a 2)))
                     (call (pipe 'sqrt
                                 (fn (a) (- a 1))
                                 (fn (a) (/ a 2)))
                      5)))
    (error (concat error-prelude "error: function composition (1) test(s) failed"))))


(defun cora-test-function-composition2 (error-prelude)
  (when (not= (thread 5
                'sqrt
                (fn (a) (- a 1))
                (fn (a) (/ a 2)))
              (call (compose (fn (a) (/ a 2))
                             (fn (a) (- a 1))
                             'sqrt)
               5))
    (error (concat error-prelude "error: function composition (2) test(s) failed"))))


(defun cora-test-string-join (error-prelude)
  (let ((s "desafortunadamente"))
    (assert-equal
      (reverse s)
      (thread s
        'string-to-list
        'reverse
        'join-chars)
      (concat error-prelude "error: string join test(s) failed"))))


(defun cora-test-curry (error-prelude)
  (letrec ((square (fn (a) (* a a)))
           (sum-squares (fn (a b)
                          (sqrt (+ (call square a)
                                   (call square b))))))
    (assert-equal
      (call sum-squares 3 4)
      (call (call (curry2 sum-squares) 3) 4)
      (concat error-prelude "error: curry test(s) failed"))))


(defun cora-test-partial (error-prelude)
  (letrec ((square (fn (a) (* a a)))
           (sum-squares (fn (a b)
                          (sqrt (+ (call square a)
                                   (call square b))))))
    (assert-equal
      (call sum-squares 3 4)
      (call (partial sum-squares 3) 4)
      (concat error-prelude "error: partial test(s) failed"))))


(defun cora-test-fold (error-prelude)
  (assert-equal
    204
    (fold (fn (acc a) (+ acc (* a a))) 0 (range (inc 8)))
    (concat error-prelude "error: fold test(s) failed"))
  (let ((input "this is a test"))
    (assert-equal
      input
      (fold ; TODO add to unit tests (Cora)
        (fn (acc a)
               (concat acc (join-chars (list a))))
        ""
        (string-to-list input))
      (concat error-prelude "error: fold test(s) failed"))))


(defun cora-test-drop-take (error-prelude)
  (assert-equal
    '(3 1 2)
    (take 3 '(3 1 2 0 5 4))
    (concat error-prelude "error: take test(s) failed"))
  (assert-equal
    '(0 5 4)
    (drop 3 '(3 1 2 0 5 4))
    (concat error-prelude "error: drop test(s) failed")))


(defun cora-test-zip (error-prelude)
  (assert-equal
    '((3 0) (1 1) (2 2) (5 3))
    (zip '(3 1 2 5 4)
         '(0 1 2 3))
    (concat error-prelude "error: drop test(s) failed")))


(defun cora-test-enumerate-partition (error-prelude)
  (assert-equal
    '((0 3) (1 1) (2 2) (3 5) (4 4))
    (enumerate '(3 1 2 5 4))
    (concat error-prelude "error: drop test(s) failed"))
  (assert-equal
    '((5 3 1) (6 4 0 2 8))
    (partition 'odd? '(8 1 2 0 3 5 4 6))
    (concat error-prelude "error: drop test(s) failed")))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run unit tests

(defun cora-test-run-tests (&rest tests)
  (letrec ((prelude "cora-test ... ")
           (execute-tests (fn (fns)
                            (cond ((null fns) nil)
                                  (t (call (car fns) prelude)
                                     (call execute-tests (cdr fns)))))))
    (message (concat prelude "running tests..."))
    (call execute-tests tests)
    (message (concat prelude "passed all tests"))))


(cora-test-run-tests
  'cora-test-compound
  'cora-test-compound2
  'cora-test-compound3
  'cora-test-function-composition
  'cora-test-function-composition2
  'cora-test-string-join
  'cora-test-curry
  'cora-test-partial
  'cora-test-fold
  'cora-test-drop-take
  'cora-test-zip
  'cora-test-enumerate-partition)





(provide 'cora-test)
;;; cora-test.el ends here
