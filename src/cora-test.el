;;; cora-test.el --- Unit tests for Cora functional programming -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 30, 2023
;; Modified: September 17, 2023
;; Version: 0.2.29
;; Keywords: language extensions internal lisp tools emacs
;; Homepage: https://github.com/usefulmove/cora
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Cora unit tests
;;
;;  Source code: ~/repos/cora/src/cora.el
;;
;;; Code:

; load Cora language
(add-to-list 'load-path "~/repos/cora/src/")
(require 'cora)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test definitions

(defun cora-test-compound (error-prelude)
  (when (not (zero? (- 204 (sum (map
                                  (\ (a) (* a a))
                                  (range (inc 8)))))))
    (error (concat error-prelude "error: compound test(s) failed")))
  (assert-equal
    (call (cache 'inc) 8)
    9
    (error (concat error-prelude "error: compound test(s) failed")))
  (assert-equal
    (range 1 3)
    '(1 2)
    (error (concat error-prelude "error: compound test(s) failed"))))


(defun cora-test-compound2 (error-prelude)
  (assert-equal
    (prod (filter 'odd? (map
                          (\ (a) (* a a a))
                          (range (dec 10)))))
    1157625
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (init '(3 1 2))
    '(3 1)
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (end '(3 1 2))
    2
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (all? 'even? (map
                   (\ (a) (* 2 a))
                   (range (inc 31))))
    t
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (gcd 18 30 12)
    6
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (not (any? 'ascii-numeric? (list 46 47 58 59)))
    (all? 'ascii-numeric? (list ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    (concat error-prelude "error: compound2 test(s) failed"))
  (assert-equal
    (flatten '(3 1 (2 1 2)))
    '(3 1 2 1 2)
    (concat error-prelude "error: compound2 test(s) failed")))


(defun cora-test-compound3 (error-prelude)
  (when (any? 'even? '(3 1 5 9 7))
    (error (concat error-prelude "error: compound3 test(s) failed"))))


(defun cora-test-function-composition (error-prelude)
  (when (not-equal? (thread 5
                      'sqrt
                      (\ (a) (- a 1))
                      (\ (a) (/ a 2)))
                    (call (pipe 'sqrt
                                (\ (a) (- a 1))
                                (\ (a) (/ a 2)))
                     5))
    (error (concat error-prelude "error: function composition (1) test(s) failed"))))


(defun cora-test-function-composition2 (error-prelude)
  (when (not= (thread 5
                'sqrt
                (\ (a) (- a 1))
                (\ (a) (/ a 2)))
              (call (compose (\ (a) (/ a 2))
                             (\ (a) (- a 1))
                             'sqrt)
               5))
    (error (concat error-prelude "error: function composition (2) test(s) failed"))))


(defun cora-test-string-join (error-prelude)
  (assert-equal
    (join '("3" "1" "2" "5" "4"))
    "31254"
    (concat error-prelude "error: string join test(s) failed"))
  (assert-equal
    (join '("3" "1" "2" "5" "4") ", ")
    "3, 1, 2, 5, 4"
    (concat error-prelude "error: string join test(s) failed"))
  (let ((s "desafortunadamente"))
    (assert-equal
      (thread s
        'string-to-list
        'reverse
        'join-chars)
      (reverse s)
      (concat error-prelude "error: string join test(s) failed"))))


(defun cora-test-curry (error-prelude)
  (letrec ((square (\ (a) (* a a)))
           (sum-squares (lambda (a b)
                          (sqrt (+ (call square a)
                                   (call square b))))))
    (assert-equal
      (call sum-squares 3 4)
      (call (call (curry2 sum-squares) 3) 4)
      (concat error-prelude "error: curry test(s) failed"))))


(defun cora-test-partial (error-prelude)
  (letrec ((square (\ (a) (* a a)))
           (sum-squares (lambda (a b)
                          (sqrt (+ (call square a)
                                   (call square b))))))
    (assert-equal
      (call sum-squares 3 4)
      (call (partial sum-squares 3) 4)
      (concat error-prelude "error: partial test(s) failed"))))


(defun cora-test-fold (error-prelude)
  (assert-equal
    (foldl (lambda (acc a) (+ acc (* a a))) 0 (range (inc 8)))
    204
    (concat error-prelude "error: fold test(s) failed"))
  (let ((input "this is a test"))
    (assert-equal
      (fold
        (lambda (acc a)
               (concat acc (join-chars (list a))))
        ""
        (string-to-list input))
      input
      (concat error-prelude "error: fold test(s) failed"))))


(defun cora-test-drop-take (error-prelude)
  (assert-equal
    (take 3 '(3 1 2 0 5 4))
    '(3 1 2)
    (concat error-prelude "error: take test(s) failed"))
  (assert-equal
    (takebut 2 '(3 1 2 0 5 4))
    '(3 1 2 0)
    (concat error-prelude "error: take test(s) failed"))
  (assert-equal
    (drop 3 '(3 1 2 0 5 4))
    '(0 5 4)
    (concat error-prelude "error: drop test(s) failed"))
  (assert-equal
    (dropbut 2 '(3 1 2 0 5 4))
    '(5 4)
    (concat error-prelude "error: drop test(s) failed")))


(defun cora-test-zip (error-prelude)
  (assert-equal
    (zip '(3 1 2 5 4)
         '(0 1 2 3))
    '((3 . 0) (1 . 1) (2 . 2) (5 . 3))
    (concat error-prelude "error: zip test(s) failed")))


(defun cora-test-enumerate-partition (error-prelude)
  (assert-equal
    (enumerate '(3 1 2 5 4))
    '((0 . 3) (1 . 1) (2 . 2) (3 . 5) (4 . 4))
    (concat error-prelude "error: enumerate test(s) failed"))
  (assert-equal
    (partition 'odd? '(8 1 2 0 3 5 4 6))
    '((5 3 1) (6 4 0 2 8))
    (concat error-prelude "error: enumerate test(s) failed")))


(defun cora-test-tally (error-prelude)
  (assert-equal
    (let ((s "As twilight cascaded upon the horizon, the iridescent hues of
              amaranthine skies caressed the gentle whispers of the zephyr,
              weaving an ephemeral symphony of love that intertwined the souls
              of all living beings in the tender embrace of nature's eternal
              harmony.")
          (get-count (lambda (key counts)
                       (cdr (assoc key counts)))))
      (call get-count ?e (tally (string-to-list s))))
    33
    (concat error-prelude "error: drop test(s) failed")))


(defun cora-test-do (error-prelude)
  (assert-equal
    (let ((cnt 0))
      (do (setq cnt (inc cnt))
          (setq cnt (inc cnt))
          (setq cnt (inc cnt)))
      cnt)
    3
    (concat error-prelude "error: do test(s) failed")))


(defun cora-test-for-comprehension (error-prelude)
  (assert-equal
    (for-comp ((a (range (inc 8)))) (* a a a))
    '(0 1 8 27 64 125 216 343 512)
    (concat error-prelude "error: for comprehension test(s) failed"))
  (assert-equal
    (for-comp ((pair (enumerate '(3 1 2))))
      (let ((i (car pair))
            (a (cdr pair)))
        (* i (* a a))))
    '(0 1 8)
    (concat error-prelude "error: for comprehension test(s) failed"))
  (assert-equal
    (for-comp ((i (range 3))
               (j (range 3)))
      (cons i j))
    '((0 . 0) (0 . 1) (0 . 2) (1 . 0) (1 . 1) (1 . 2) (2 . 0) (2 . 1) (2 . 2))
    (concat error-prelude "error: for comprehension test(s) failed")))


(defun cora-test-equality (error-prelude)
  (assert-equal
    (not= 1 1.0 1) ; nil
    (eq? 1 1.0) ; nil
    (concat error-prelude "error: equality test(s) failed"))
  (assert-equal
    (not-eq? 1 1.0) ; t
    (not-equal? 1 1.0) ; t
    (concat error-prelude "error: equality test(s) failed"))
  (let ((a "eight")
        (b "eight"))
    (assert-equal
      (equal? a b) ; t
      (not-eq? a b) ; t
      (concat error-prelude "error: equality test(s) failed")))
  (assert-equal
    (not= 1 1.0) ; nil
    (not-equal? 1 1) ; nil
    (concat error-prelude "error: equality test(s) failed")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run unit tests

(defun cora-test-run-tests (&rest tests)
  (letrec ((prelude "cora-test ... ")
           (execute-tests (lambda (fns)
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
  'cora-test-enumerate-partition
  'cora-test-tally
  'cora-test-do
  'cora-test-for-comprehension
  'cora-test-equality)



(provide 'cora-test)
;;; cora-test.el ends here
