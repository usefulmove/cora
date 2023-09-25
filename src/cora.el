;;; cora.el --- Cora functional programming -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds <duane.edmonds@gmail.com>
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 23, 2023
;; Modified: September 24, 2023
;; Version: 0.2.32
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


;; \
(defmacro \ (bindings &rest body)
  "Anonymous function shorthand."
  `(lambda ,bindings ,@body))


;; for-comp
(defmacro for-comp (bindings &rest body)
  "Comprehension. Bind variables in BINDINGS and iterate BODY over binding
permutations to generate list of mapped results."
  (let ((current-binding (car bindings))
        (remaining-bindings (cdr bindings)))
    (if (null remaining-bindings)
      `(mapcar (lambda ,(list (car current-binding)) ,@body) ,(cadr current-binding))
      `(cl-mapcan
        (lambda ,(list (car current-binding))
          (for-comp ,remaining-bindings ,@body))
        ,(cadr current-binding)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

;; foldl :: (U -> T -> U) -> U -> [T] -> U
(defun foldl (f acc lst)
  "Fold (reduce) list (LST) using applied function F starting with initial value
  ACC for the accumulator."
  (cond ((null lst) acc)
        (t (foldl f (funcall f acc (car lst)) (cdr lst)))))


;; fold :: (U -> T -> U) -> U -> [T] -> U
(fset 'fold 'foldl)


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


;; range :: number -> (optional) number -> [number]
(defun range (&rest args)
  "Generate a list of values from FROM (inclusive) to TO (non-inclusive)."
  (let ((from (if (= 1 (length args))
                0
                (car args)))
        (to (if (= 1 (length args))
              (car args)
              (cadr args)))
        (step (if (= 3 (length args))
                (caddr args)
                1)))
    (cond ((>= from to) '())
          (t (cons from (range (+ step from) to step))))))


;; for-each :: (T -> ?) -> [T] -> nil (IMPURE)
(defun for-each (f lst)
  (unless (null lst)
    (funcall f (car lst))
    (for-each f (cdr lst))))


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


;; char-to-int :: char -> integer
(defun char-to-int (c)
  "Convert numeric character (C) to its numeric equivalent. Return -1 if
character does not represent an integer value."
  (if (member c '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    (- c ?0)
    -1))


;; char-to-ord :: char -> integer
(defun char-to-ord (c)
  "Convert character (C) to its ordinal value."
  c)


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


;; zip :: [T] -> [U] -> [[T . U]]
(defun zip (lst1 lst2)
  "Zip two lists (LST1) and (LST2) together and return an association list in
which the first element comes from LST1 and the second element comes from LST2.
The resulting zipped association list will have the same length as the shortest
of the two provided lists."
  (cond ((or (null lst1)
             (null lst2)) '())
        (t (cons (cons (car lst1)
                       (car lst2))
                 (zip (cdr lst1)
                      (cdr lst2))))))


;; enumerate :: [T] -> [[integer . T]]
(defun enumerate (lst)
  "Enumerate the list (LST) by returning an association list whose elements are
the element index (0-based) and the element itself."
  (zip (range (length lst))
       lst))


;; partition :: (T -> boolean) -> [T] -> [[T] [T]]
(defun partition (f lst)
  "Partition list (LST) into two lists using predicate function (F). The return
value is a list of lists with the first element is the list of elements for
which F returns t (true), and the second element is the list of elements for
which F returns nil (false)."
  (foldl
    (lambda (acc e)
      (if (call f e)
        (list (cons e (car acc)) ; match - add to first element of accumulator
              (cadr acc))
        (list (car acc)
              (cons e (cadr acc))))) ; no match - added to second element of accumulator
    '(() ())
    lst))


;; tally :: [T] -> [T . integer]
(defun tally (lst &optional map)
  "Count elements in list (LST) and return an association list with
key-count pairs."
  (setq counts-hash (if map
                      map
                      (make-hash-table :test 'equal)))

  (if (null lst)
    counts-hash
    (do
      (puthash ; add first element to table
        (car lst)
        (+ 1 (gethash (car lst) counts-hash 0))
        counts-hash)
      (tally (cdr lst) counts-hash))) ; recursively run on rest of list (tail recursion)

  (let (counts '())
    (maphash ; convert hash table to association list
      (lambda (key value)
        (setq counts (cons (cons key value) counts)))
      counts-hash)
    counts))


;; join :: [string] -> (optional) string -> string
(defun join (lst &optional sep)
  "Concatenate the list of strings (LST) into one using the provided
separator (SEP)."
  (foldl
    (lambda (acc s)
      (concat acc sep s))
    (car lst)
    (cdr lst)))


;; memoize :: (T -> U) -> (T -> U)
(defun memoize (f)
  "Return a memoized version of the provided unary function (F)."
  (let ((hmap (make-hash-table :test 'equal)))
    (lambda (a)
      (let ((cached-value (gethash a hmap :none)))
        (if (not-equal? :none cached-value) cached-value
          (let ((return (call f a)))
            (puthash a return hmap)
            return))))))



(provide 'cora)
;;; cora.el ends here
