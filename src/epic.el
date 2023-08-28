;;; epic.el --- Functional programming algorithms library -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Robert Duane Edmonds
;;
;; Author: Duane <dedmonds@gmail.com>
;; Maintainer: Duane <dedmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: August 27, 2023
;; Version: 0.0.3
;; Keywords: extensions internal lisp tools
;; Homepage: https://github.com/dedmonds/epic
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description:  functional programming algorithms
;;
;;; Code:


;; map :: (T -> U) -> [T] -> [U]
(defun _map (f lst)
  "Map function over elements of list and return updated list."
  (cond ((null lst) '()) ; end of list?
        (t (cons (funcall f (car lst))
                 (_map f (cdr lst))))))


;; fold :: (U -> T -> U) -> U -> [T] -> U
(defun _fold (f acc lst)
  "Fold (reduce) list using applied function."
  (cond ((null lst) acc)
        (t (_fold f (funcall f acc (car lst)) (cdr lst)))))


(defun _filter (f lst)
  "Filter list using applied function."
  (cond ((null lst) '())
        ((not (funcall f (car lst))) (_filter f (cdr lst)))
        (t (cons (car lst)
                 (_filter f (cdr lst))))))


;; partial :: (... -> T -> U) -> [...] -> (T -> U)
(defun _partial (&rest args)
  "Return unary function when passed an n-ary function and (- n 1) arguments."
  (let ((f (car args))
        (fargs (cdr args)))
    (lambda (a)
      (apply f (append fargs (list a))))))


;; thread :: T -> [(T -> T)] -> T
(defun _thread (&rest args)
  "Thread a seed value through the function defined by the composition of a
  list of functions. This higher-order function can simplify (and make more
  expressive) deeply nested compositional patterns."
  (let ((seed (car args))
        (fns (cdr args)))
    (_fold
      (lambda (acc f)
        (funcall f acc))
      seed
      fns)))
;;
;;(_thread 8
;;  '(lambda (n) (* n n n)) ; cube (note - lambda does not have to be quoted)
;;  'number-to-string
;;  'message) ; => "512"


;; curry2 :: (T -> U -> V) -> (T -> (U -> V))
(defun _curry2 (f)
  "Return curried binary function."
  (lambda (a)
    (lambda (b) (funcall f a b))))


;; range :: number -> [number]
(defun _range (n)
  (cond ((= 0 n) '())
        (t (append (_range (- n 1))
                   (list (- n 1))))))


;; inc :: number -> number
(defun _inc (n)
  "Increment number."
  (+ 1 n))


;; dec :: number -> number
(defun _dec (n)
  "Decrement number."
  (- n 1))


;; even? :: number -> boolean
(defun _even? (n)
  (= 0 (mod n 2)))


;; odd? :: number -> boolean
(defun _odd? (n)
  (= 1 (mod n 2)))


;; zero? :: number -> boolean
(defun _zero? (n)
  (= 0 n))


;; sum :: [T] -> T
(defun _sum (lst)
  (apply '+ lst))


;; prod :: [T] -> T
(defun _prod (lst)
  (apply '* lst))


;; all? :: (T -> boolean) -> [T] -> boolean
(defun _all? (f lst)
  "Check that function applied to all values in the list returns true."
  (cond ((null lst) t)
        ((not (funcall f (car lst))) nil)
        (t (_all? f (cdr lst)))))


;; any? :: (T -> boolean) -> [T] -> boolean
(defun _any? (f lst)
  "Check that function applied to at least one value in the list returns true."
  (cond ((null lst) nil)
        ((funcall f (car lst)) t)
        (t (_any? f (cdr lst)))))


;; init :: [T] -> [T]
(defun _init (lst)
  "Return all elements of list except first."
  (reverse (cdr (reverse lst))))


;; last :: [T] -> [T]
(defun _last (lst)
  "Return the last element of the list."
  (car (reverse lst)))


;; join-chars :: [char] -> string
(defun _join-chars (chars)
  "Join the elements of list of characters into a string."
  (apply 'string chars))


;; gcd :: int -> int -> int
(defun _gcd (a b)
  "Calculate the greatest common divisor."
  (cond ((= 0 b) a)
        (t (_gcd b (mod a b)))))




(provide 'epic)
;;; epic.el ends here
