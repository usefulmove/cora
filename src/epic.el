;;; epic.el --- Functional programming algorithms library -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Robert Duane Edmonds
;;
;; Author: Duane <dedmonds@G3>
;; Maintainer: Duane <dedmonds@G3>
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

;; fold ::
;(defun fold (f acc lst)
;  )

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

;(defun all? (lst)
;  )

;(defun any? (lst)
;  )

;(defun init (lst)
;  )

(init '(3 1 2 5 4))

;(defun prod (lst)
;  )


(provide 'epic)
;;; epic.el ends here
