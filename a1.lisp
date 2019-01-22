; Copyright (c) 2019, Nathan Klapstien
; All rights reserved.
; So much recursion ;-;

; Q:1
; This function checks whether a given list of integers L is sorted in
; strictly increasing order. It returns T if L is sorted, and NIL otherwise.
; Lists with fewer than two elements are considered sorted.
(defun issorted (L)
  (if (nth 0 L)
    (if (nth 1 L)
      (if (< (first L) (nth 1 L))
        (issorted (cdr L))
        ())
      T)
    T))

; Q:2
;Given a nonnegative integer N, produce the list of all integers from 1 up to
; and including N.
(defun numbers(N)
  (if (>= 0 N)
    ()
    (if (= 1 N)
      (cons 1 ())
      (append (numbers (- N 1)) (cons N ())))))

; Q:3
; Palindrome checks if a given list L of atoms is a palindrome.
; i.e. it reads the same from the front and the back.
(defun palindrome (L)
  (if (null L)
    T
    (if (eq (first L) (car (last L)))
      (palindrome (reverse (cdr (reverse (cdr L)))))
      ())))

; Q:4.1
; Function replace1 replaces all instances of Atom1 by Atom2 in elements of
; List. Sublists should be left as they are do not replace anything inside
;a sublist.
(defun replace1 (Atom1 Atom2 L)
  (if (null L)
    ()
    (if (eq Atom1 (first L))
      (cons Atom2 (replace1 Atom1 Atom2 (cdr L)))
      (cons (first L) (replace1 Atom1 Atom2 (cdr L))))))




; Q:4.2
; The function replace2 also replaces Atom1 by Atom2. However, this function
; replaces recursively in all sublists.
(defun replace2 (Atom1 Atom2 L)
  (if (null L)
    ()
    (if (atom (first L))
      (if (eq Atom1 (first L))
        (cons Atom2 (replace2 Atom1 Atom2 (cdr L)))
        (cons (first L) (replace2 Atom1 Atom2 (cdr L))))
      (cons (replace2 Atom1 Atom2 (first L)) (replace2 Atom1 Atom2 (cdr L))))))


; Q:5
; L1 and L2 are lists of atoms. In these lists, no atom appears more than once.
; common counts how many atoms are contained in both L1 and L2.
(defun common (L1 L2)
  (if (or (null L1) (null L2))
    0
    (if (member (first L1) L2)
      (+ 1 (common (cdr L1) L2))
      (+ 0 (common (cdr L1) L2)))))

; Q:6
; This function should implement the greedy algorithm for the set cover problem.
; This problem and the algorithm are described below. The Wikipedia article
; on set cover also explains the problem (in much more detail than we need).
;
; In (setcover N S), N is a nonnegative integer, and S is a set of subsets of
; the numbers U = (1 2 ... N). The set cover problem asks to find a (small)
; number of subsets in S such that their union covers U. This means that every
; number in U is contained in at least one of the subsets in the solution.
;
; Example:
;
; (let
;     ((S '((1 2 3) (2 4) (3 4) (2 5) (4 5))))
;     (setcover 5 S)
; )
;
; A solution: ((1 2 3) (4 5))
;
; Explanations: N = 5, so U = (1 2 3 4 5). S consists of some subsets of
; (1 2 3 4 5). We are looking for some small number of those subsets that
; together cover all the five numbers.
;
; The best solution uses only two subsets, (1 2 3) and (4 5). Another solution,
; with three subsets, is ((1 2 3) (2 4) (2 5)). Yet another solution is
; ((1 2 3) (2 4) (3 4) (2 5)). However, in this solution you could remove
; either (2 4) or (3 4) and get a smaller solution that still covers all of U.
;
; Solving the set cover problem optimally means to find the smallest number of
; subsets of S that cover U. (Number of sets, not size of sets.) Unfortunately,
; this problem is NP-hard, and therefore no efficient algorithm is known.
;
; Instead of the optimal solution, your program should compute and return the
; greedy solution - a small set of subsets that covers U and is computed by the
; so-called greedy algorithm below. This algorithm is also described on the
; wikipedia page.
;
; The basic idea is to solve the problem in several rounds. In each round, we
; select one more subset from S until we have a complete cover. We pick a
; subset that contains as many of the still missing numbers as possible.
;
; Assume that we still have some of the numbers in (1 2 ... N) left to cover.
; We consider each subset Si in S, and count how many of these numbers would
; be covered by Si. Then we greedily pick a subset that covers the most.

; given a set of integers R and a list of integer sets L
; return the set within L the best covers R
(defun bestset (R L)
  (if (null (nth  1 L))  ; we are at last element within a list, return it
    (first L)
    (if (= (common R (first L)) (common R (nth 1 L))) ; prioritize returning the first element
      (bestset R (cons (first L) (cddr L)))
      (if (> (common R (first L)) (common R (nth 1 L)))
        (bestset R (cons (first L) (cddr L)))
        (bestset R (cdr L))))))



; compute the difference of two lists e.g. l1-l2
; used in setcoverrec
(defun listdiff (L1 L2)
  (if L1
    (if (member (car L1) L2)
      (listdiff (cdr L1) L2)
      (cons (car L1) (listdiff (cdr L1) L2)))))

; the recursive component to setcover. This was used as the arguement
; structure of setcover is hard to make recursive.
(defun setcoverrec (R L)
  (if (null R) ()
    (cons (bestset R L) (setcoverrec (listdiff R (bestset R L)) (listdiff L (cons (bestset R L) ()))))))

(defun setcover (N L)
  (setcoverrec (numbers N) L))
