;;; cora.el --- Cora functional programming -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: September 12, 2023
;; Version: 0.2.20
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


;; not= :: T -> U -> V -> ... -> boolean
(defmacro not= (&rest args)
  "Test that objects are not numerically equal."
  `(not (= ,@args)))


;; equal? :: T -> U -> boolean
(defmacro equal? (a b)
  "Test that objects A and B have equal components."
  `(equal ,a ,b))


;; not-equal? :: T -> U -> boolean
(defmacro not-equal? (a b)
  "Test that objects A and B do not have equal components."
  `(not (equal? ,a ,b)))


;; eq? :: T -> U -> boolean
(defmacro eq? (a b)
  "Test that objects A and B are the same object."
  `(eq ,a ,b))


;; not-eq? :: T -> U -> boolean
(defmacro not-eq? (a b)
  "Test that objects A and B are not the same object."
  `(not (eq ,a ,b)))


;; call
(defmacro call (f &rest args)
  `(funcall ,f ,@args))


;; assert-equal :: sexp -> sexp -> string -> nil (IMPURE)
(defmacro assert-equal (exp1 exp2 error-msg)
  `(when (not (equal ,exp1 ,exp2))
  (error ,error-msg)))


;; map :: (T -> U) -> [T] -> [U]
(defmacro map (f lst)
  `(mapcar ,f ,lst))


;; filter :: (T -> boolean) -> [T] -> [T]
(defmacro filter (f lst)
  `(cl-remove-if-not ,f ,lst))


;; flatten :: [[T]] -> [T]
(fset 'flatten '-flatten)


;; do
(defmacro do (&rest forms)
  "Evaluate body FORMS sequentially and return value of the last one."
  `(let () ,@forms))


;; _
(defmacro _ (exp)
  "Unary anonymous function shorthand macro."
  `(lambda (%) ,exp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; fold :: (U -> T -> U) -> U -> [T] -> U
(defun fold (f acc lst)
  "Fold (reduce) list (LST) using applied function F starting with initial value
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
list of functions (FNS). This higher-order function can simplify (and make more
expressive) deeply nested compositional patterns."
  (cond ((null fns) seed)
        (t (apply 'thread (cons (funcall (car fns) seed)
                                (cdr fns))))))


;; compose :: [(T -> T)] -> (T -> T)
(defun compose (&rest fns)
  "Create composed function constructed of function arguments (FNS)."
  (cond ((null fns) 'identity)
        (t (let ((last-fn (car fns))
                        (rest-fn (apply 'compose (cdr fns))))
                (lambda (seed)
                (funcall last-fn (funcall rest-fn seed)))))))


;; pipe :: [(T -> T)] -> (T -> T)
(defun pipe (&rest fns)
  "Create composed function constructed of function arguments (FNS). The order
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
  "Sum elements of list (LST)."
  (apply '+ lst))


;; prod :: [T] -> T
(defun prod (lst)
  "Calculate the product of elements of list (LST)."
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
list (LST) returns true."
  (cond ((null lst) nil)
        ((funcall f (car lst)) t)
        (t (any? f (cdr lst)))))


;; init :: [T] -> [T]
(defun init (lst)
  "Return all elements of list (LST) except first."
  (reverse (cdr (reverse lst))))


;; end :: [T] -> [T]
(defun end (lst)
  "Return the last element of the list (LST)."
  (car (reverse lst)))


;; join-chars :: [char] -> string
(defun join-chars (chars)
  "Join the elements of list of characters (CHARS) into a string."
  (apply 'string chars))


;; gcd :: int -> int -> ... -> int (n-ary)
(defun gcd (&rest args)
  "Calculate the greatest common denominator of number arguments (ARGS)."
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
  "Take first N elements from list (LST)."
  (cond ((null lst) '())
        ((= 0 n) '())
        (t (cons (car lst)
                 (take (- n 1) (cdr lst))))))


;; takebut :: int -> [T] -> [T]
(defun takebut (n lst)
  "Take all but last N elements from list (LST)."
  (take (- (length lst) n) lst))


;; drop :: int -> [T] -> [T]
(defun drop (n lst)
  "Drop first N elements from list (LST)."
  (cond ((null lst) '())
        ((= 0 n) lst)
        (t (drop (- n 1) (cdr lst)))))


;; dropbut :: int -> [T] -> [T]
(defun dropbut (n lst)
  "Drop all but last N elements from list (LST)."
  (drop (- (length lst) n) lst))


;; zip :: [T] -> [U] -> [[T U]]
(defun zip (lst1 lst2)
  "Zip two lists (LST1) and (LST2) together and return a list of lists in which
the first element comes from LST1 and the second element comes from LST2. The
resulting zipped list will have the same length as the shortest of the two
lists provided."
  (cond ((or (null lst1)
             (null lst2)) '())
        (t (cons (list (car lst1)
                       (car lst2))
                 (zip (cdr lst1)
                      (cdr lst2))))))


;; enumerate :: [T] -> [[integer T]]
(defun enumerate (lst)
  "Enumerate the list (LST) by returning a list whose elements are the element
number (0-based) and the element itself."
  (zip (range (length lst))
       lst))


;; partition :: (T -> boolean) -> [T] -> [[T] [T]]
(defun partition (f lst)
  "Partition list (LST) into two lists using predicate function (F). The return
value is a list of lists with the first element is the list of elements for
which F returns t (true), and the second element is the list of elements for
which F returns nil (false)."
  (fold
    (lambda (acc e)
      (if (call f e)
          (list (cons e (car acc)) ; match - add to first element of accumulator
                (cadr acc))
          (list (car acc)
                (cons e (cadr acc))))) ; no match - added to second element of accumulator
    '(() ())
    lst))


;; counter :: [T] -> #(T -> integer)
(defun counter (lst &optional map)
  "Count elements in list (LST) and return a hash table with counts."
  (setq counts (if map map
                   (make-hash-table :test 'equal)))
  (if (null lst) counts
      (do
        (puthash ; add first element to table
          (car lst)
          (+ 1 (gethash (car lst) counts 0))
          counts)
        (counter (cdr lst) counts)))) ; recursively run on rest of list (tail recursion)


;; join :: [string] -> (optional) string -> string
(defun join (lst &optional sep)
  "Concatenate the list of strings (LST) into one using the provided
separator (SEP)."
  (fold
    (lambda (acc s)
      (concat acc sep s))
    (car lst)
    (cdr lst)))



(provide 'cora)
;;; cora.el ends here
