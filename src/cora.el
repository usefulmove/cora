;;; cora.el --- Cora functional programming -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: September 9, 2023
;; Version: 0.2.13
;; Keywords: language extensions internal lisp tools emacs
;; Homepage: https://github.com/usefulmove/cora
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description: Cora functional programming
;;
;;  Unit tests: ~/repos/cora/src/cora-test.el
;;
;;; Code:

(require 'cl-lib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

; equal? :: sexp -> sexp -> boolean
(defmacro equal? (exp exp2)
  `(equal ,exp ,exp2))

;; not= :: sexp -> sexp -> boolean
(defmacro not= (exp exp2)
  `(not (= ,exp ,exp2)))

; call
(defmacro call (f &rest args)
  `(funcall ,f ,@args))

; assert-equal :: sexp -> sexp -> string -> nil (IMPURE)
(defmacro assert-equal (exp1 exp2 error-msg)
  `(when (not (equal ,exp1 ,exp2))
     (error ,error-msg)))

; map :: (T -> U) -> [T] -> [U]
(defmacro map (f lst)
  `(mapcar ,f ,lst))

; filter :: (T -> boolean) -> [T] -> [T]
(defmacro filter (f lst)
  `(cl-remove-if-not ,f ,lst))

; flatten :: [[T]] -> [T]
(fset 'flatten '-flatten)

(defmacro fn (&rest cdr)
  "Return an anonymous function.
Under dynamic binding, a call of the form (lambda ARGS DOCSTRING
INTERACTIVE BODY) is self-quoting; the result of evaluating the
lambda expression is the expression itself.  Under lexical
binding, the result is a closure.  Regardless, the result is a
function, i.e., it may be stored as the function value of a
symbol, passed to `funcall' or `mapcar', etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of Lisp expressions.

\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 2) (indent defun)
           (debug (&define lambda-list lambda-doc
                           [&optional ("interactive" interactive)]
                           def-body)))
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; fold :: (U -> T -> U) -> U -> [T] -> U
(defun fold (f acc lst)
  "Fold (reduce) list LST using applied function F starting with initial value
  ACC for the accumulator."
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
(defun thread (seed &rest fns)
  "Thread a SEED value through the function defined by the composition of the
list of functions FNS. This higher-order function can simplify (and make more
expressive) deeply nested compositional patterns."
  (cond ((null fns) seed)
        (t (apply 'thread (cons (funcall (car fns) seed)
                                (cdr fns))))))


;; compose :: [(T -> T)] -> (T -> T)
(defun compose (&rest fns)
  "Create composed function constructed of function arguments FNS."
  (cond ((null fns) 'identity)
        (t (let ((last-fn (car fns))
                 (rest-fn (apply 'compose (cdr fns))))
             (lambda (seed)
               (funcall last-fn (funcall rest-fn seed)))))))


;; pipe :: [(T -> T)] -> (T -> T)
(defun pipe (&rest fns)
  "Create composed function constructed of function arguments FNS. The order
of function application is reversed from the compose function."
  (apply 'compose (reverse fns)))


;; curry2 :: (T -> U -> V) -> (T -> (U -> V))
(defun curry2 (f)
  "Return curried binary function F."
  (lambda (a)
    (lambda (b) (funcall f a b))))


;; range :: number -> [number]
(defun range (n)
  "Generate a list of values from 0 to (- N 1)."
  (cond ((= 0 n) '())
        (t (append (range (- n 1))
                   (list (- n 1))))))


;; inc :: number -> number
(defun inc (n)
  "Increment number N."
  (+ 1 n))


;; dec :: number -> number
(defun dec (n)
  "Decrement number N."
  (- n 1))


;; even? :: number -> boolean
(defun even? (n)
  "Is N even?"
  (= 0 (mod n 2)))


;; odd? :: number -> boolean
(defun odd? (n)
  "Is N odd?"
  (= 1 (mod n 2)))


;; zero? :: number -> boolean
(defun zero? (n)
  "Is N equal to zero?"
  (= 0 n))


(defun ascii-numeric? (c)
  "Check is C a valid ascii numeric character?"
  (and (>= c ?0) (<= c ?9)))


;; sum :: [T] -> T
(defun sum (lst)
  "Sum elements of list LST."
  (apply '+ lst))


;; prod :: [T] -> T
(defun prod (lst)
  "Calculate the product of elements of list LST."
  (apply '* lst))


;; all? :: (T -> boolean) -> [T] -> boolean
(defun all? (f lst)
  "Check that function applied to all values in the list returns true."
  (cond ((null lst) t)
        ((not (funcall f (car lst))) nil)
        (t (all? f (cdr lst)))))


;; any? :: (T -> boolean) -> [T] -> boolean
(defun any? (f lst)
  "Check that function (F) applied to at least one value in the
  list LST returns true."
  (cond ((null lst) nil)
        ((funcall f (car lst)) t)
        (t (any? f (cdr lst)))))


;; init :: [T] -> [T]
(defun init (lst)
  "Return all elements of list LST except first."
  (reverse (cdr (reverse lst))))


;; end :: [T] -> [T]
(defun end (lst)
  "Return the last element of the list LST."
  (car (reverse lst)))


;; join-chars :: [char] -> string
(defun join-chars (chars)
  "Join the elements of list of characters CHARS into a string."
  (apply 'string chars))


;; gcd :: int -> int -> ... -> int (n-ary)
(defun gcd (&rest args)
  "Calculate the greatest common denominator of ARGS."
  (cl-labels ((gcd (a b)
                   (cond ((= 0 b) a)
                         (t (gcd b (mod a b))))))
    (cond ((= 2 (length args)) (gcd (car args)
                                    (cadr args)))
          (t (apply 'gcd (cons (gcd (car args)
                                     (cadr args))
                                (cddr args)))))))


;; take :: int -> [T] -> [T]
(defun take (n lst)
  "Take first N elements from list LST."
  (cond ((null lst) '())
        ((= 0 n) '())
        (t (cons (car lst) (take (- n 1) (cdr lst))))))


;; takebut :: int -> [T] -> [T]
(defun takebut (n lst)
  "Take all but last N elements from list LST."
  (take (- (length lst) n) lst))


;; drop :: int -> [T] -> [T]
(defun drop (n lst)
  "Drop first N elements from list LST."
  (cond ((null lst) '())
        ((= 0 n) lst)
        (t (drop (- n 1) (cdr lst)))))


;; dropbut :: int -> [T] -> [T]
(defun dropbut (n lst)
  "Drop all but last N elements from list LST."
  (drop (- (length lst) n) lst))


;; zip :: [T] -> [U] -> [[T U]]
(defun zip (lst1 lst2)
  (cond ((or (null lst1)
             (null lst2)) '())
        (t (cons (list (car lst1)
                       (car lst2))
                 (zip (cdr lst1)
                      (cdr lst2))))))


;; enumerate :: [T] -> [[integer T]]
(defun enumerate (lst)
  (zip (range (length lst))
       lst))


;; partition :: (T -> boolean) -> [T] -> [[T] [T]]
(defun partition (f lst)
  (fold
    (lambda (acc e)
      (if (call f e)
          (list (cons e (car acc)) ; match - add to first element of accumulator
                (cadr acc))
          (list (car acc)
                (cons e (cadr acc))))) ; no match - added to second element of accumulator
    '(() ())
    lst))





(provide 'cora)
;;; cora.el ends here
