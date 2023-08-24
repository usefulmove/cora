;;; epic.el --- functional programming algorithms library -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Robert Duane Edmonds
;;
;; Author: Duane <dedmonds@G3>
;; Maintainer: Duane <dedmonds@G3>
;; Created: August 23, 2023
;; Modified: August 23, 2023
;; Version: 0.0.1
;; Keywords: extensions internal lisp tools
;; Homepage: https://github.com/dedmonds/epic
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description:  functional programming algorithms library
;;
;;; Code:

; map :: (T -> U) -> [T] -> [U]
(defun map (f lst)
  (cond ((null lst) '()) ; end of list?
        (t (cons (funcall f (car lst))
                 (map f (cdr lst))))))

(defun cube (n) (* n n n))
(map 'cube (list 8 1 2))


; curry2 :: (T -> U -> V) -> (T -> (U -> V))
(defun curry2 (f) ; TODO - troubleshoot
  (lambda (a) (lambda (b) (funcall f a b))))

(defun add (a b) (+ a b))
(add 3 2)

(map (lambda (a) (add 3 a)) '(3 1 2))

(setq curried-add (curry2 'add))

(setq add3 (funcall curried-add 3))
(funcall add3 2)

(funcall (funcall curried-add 3) 2)


(setq asdf 'map)
(funcall asdf 'cube '(8 1 2))


;(defun all? (lst)
;  )
;
;(defun any? (lst)
;  )
;
;(defun init (lst)
;  )
;
;(defun prod (lst)
;  )
;
;(defun sum (lst)
;  )

(provide 'epic)
;;; epoch.el ends here
