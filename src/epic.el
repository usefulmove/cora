;;; epic.el --- Functional programming algorithms library -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Robert Duane Edmonds
;;
;; Author: Duane <dedmonds@gmail.com>
;; Maintainer: Duane <dedmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: August 24, 2023
;; Version: 0.0.1
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
(defun map (f lst)
  "map function over elements of list"
  (cond ((null lst) '()) ; end of list?
        (t (cons (funcall f (car lst))
                 (map f (cdr lst))))))

;; fold :: (T -> U -> T) -> [U] -> T
(defun fold (f acc lst)
  "fold (reduce) list"
  (cond ((null lst) acc)
        (t (fold f (funcall f acc (car lst)) (cdr lst)))))

;; filter :: (T -> boolean) -> [T]
(defun filter (f lst)
  (cond ((null lst) '())
        ((not (funcall f (car lst))) (filter f (cdr lst)))
        (t (cons (car lst)
                 (filter f (cdr lst))))))

;; curry2 :: (T -> U -> V) -> (T -> (U -> V))
(defun curry2 (f) ; TODO - troubleshoot
  "return curried binary function"
  (lambda (a)
    (lambda (b) (funcall f a b))))

;; range :: number -> [number]
(defun range (n)
  (cond ((= 0 n) '())
        (t (append (range (- n 1))
                   (list (- n 1))))))

;; inc :: number -> number
(defun inc (n)
  "increment number"
  (+ 1 n))

;; dec :: number -> number
(defun dec (n)
  "decrement number"
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
  "check that function applied to all values in the list returns true"
  (cond ((null lst) t)
        ((not (funcall f (car lst))) nil)
        (t (all? f (cdr lst)))))

;; any? :: (T -> boolean) -> [T] -> boolean
(defun any? (f lst)
  "check that function applied to at least one value in the list returns true"
  (cond ((null lst) nil)
        ((funcall f (car lst)) t)
        (t (any? f (cdr lst)))))

;; init :: [T] -> [T]
(defun init (lst)
  "return all elements of list except first"
  (reverse (cdr (reverse lst))))

;; last :: [T] -> [T]
(defun last (lst)
  "return the last element of the list"
  (car (reverse lst)))



(provide 'epic)
;;; epic.el ends here
