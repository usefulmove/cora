;;; cora.el --- Cora programming language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: September 3, 2023
;; Version: 0.2.4
;; Keywords: extensions internal lisp tools
;; Homepage: https://github.com/usefulmove/cora
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Cora programming language
;;
;;; Code:

(require 'cl-lib)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(defmacro equal? (exp exp2)
  `(equal ,exp ,exp2))


(defmacro call (f &rest args)
  `(funcall ,f ,@args))


(defmacro assert-equal (exp1 exp2 error-msg)
  `(when (not (equal ,exp1 ,exp2))
     (error ,error-msg)))


(defmacro map (f lst)
  `(mapcar ,f ,lst))


(defmacro filter (f lst)
  `(cl-remove-if-not ,f ,lst))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; fold :: (U -> T -> U) -> U -> [T] -> U
(defun fold (f acc lst)
  "Fold (reduce) list using applied function."
  (cond ((null lst) acc)
        (t (fold f (funcall f acc (car lst)) (cdr lst)))))


;; partial :: (... -> T -> U) -> [...] -> (T -> U)
(defun partial (&rest args)
  "Return unary function when passed an n-ary function and (- n 1) arguments."
  (let ((f (car args))
        (fargs (cdr args)))
    (lambda (a)
      (apply f (append fargs (list a))))))


;; thread :: T -> [(T -> T)] -> T
(defun thread (&rest args)
  "Thread a seed value through the function defined by the composition of a
  list of functions. This higher-order function can simplify (and make more
  expressive) deeply nested compositional patterns."
  (unless (null args)
    (let ((seed (car args))
          (fns (cdr args)))
      (cond ((null fns) seed)
            (t (apply 'thread (cons (funcall (car fns) seed)
                                     (cdr fns))))))))


;; compose :: [(T -> T)] -> (T -> T)
(defun compose (&rest fns)
  "Create composed function constructed of function arguments."
  (cond ((null fns) 'identity)
        (t (let ((last-f (car fns))
                 (rest-f (apply 'compose (cdr fns))))
             (lambda (seed)
               (funcall last-f (funcall rest-f seed)))))))


;; pipe :: [(T -> T)] -> (T -> T)
(defun pipe (&rest fns)
  "Create composed function constructed of function arguments. The order of
  function application is reversed from the compose function."
  (apply 'compose (reverse fns)))


;; curry2 :: (T -> U -> V) -> (T -> (U -> V))
(defun curry2 (f)
  "Return curried binary function."
  (lambda (a)
    (lambda (b) (funcall f a b))))


;; range :: number -> [number]
(defun range (n)
  (cond ((= 0 n) '())
        (t (append (range (- n 1))
                   (list (- n 1))))))


;; inc :: number -> number
(defun inc (n)
  "Increment number."
  (+ 1 n))


;; dec :: number -> number
(defun dec (n)
  "Decrement number."
  (- n 1))


;; even? :: number -> boolean
(defun even? (n)
  (= 0 (mod n 2)))


;; odd? :: number -> boolean
(defun odd? (n)
  (= 1 (mod n 2)))


;; zero? :: number -> boolean
(defun zero? (n)
  (= 0 n))


;; sum :: [T] -> T
(defun sum (lst)
  (apply '+ lst))


;; prod :: [T] -> T
(defun prod (lst)
  (apply '* lst))


;; all? :: (T -> boolean) -> [T] -> boolean
(defun all? (f lst)
  "Check that function applied to all values in the list returns true."
  (cond ((null lst) t)
        ((not (funcall f (car lst))) nil)
        (t (all? f (cdr lst)))))


;; any? :: (T -> boolean) -> [T] -> boolean
(defun any? (f lst)
  "Check that function applied to at least one value in the list returns true."
  (cond ((null lst) nil)
        ((funcall f (car lst)) t)
        (t (any? f (cdr lst)))))


;; init :: [T] -> [T]
(defun init (lst)
  "Return all elements of list except first."
  (reverse (cdr (reverse lst))))


;; end :: [T] -> [T]
(defun end (lst)
  "Return the last element of the list."
  (car (reverse lst)))


;; join-chars :: [char] -> string
(defun join-chars (chars)
  "Join the elements of list of characters into a string."
  (apply 'string chars))


;; gcd :: int -> int -> ... -> int (n-ary)
(defun gcd (&rest args)
  "Calculate the greatest common denominator."
  (cl-labels ((gcd (a b)
                   (cond ((= 0 b) a)
                         (t (gcd b (mod a b))))))
    (cond ((= 2 (length args)) (gcd (car args)
                                    (cadr args)))
          (t (apply 'gcd (cons (gcd (car args)
                                     (cadr args))
                                (cddr args)))))))




(provide 'cora)
;;; cora.el ends here
