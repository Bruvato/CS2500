#lang htdp/isl+

; A [Comparison X] is a [X X -> Number]
; Interpretation: A Comparison function allows us to compare two inputs,
; with: a negative output indicating the first input is less than the second input,
;       a zero output     indicating the two inputs are equal,
;       a positive output indicating the first input is greater than the second input,
; according to some ordering.  The exact numerical values don't matter, but
; only whether they are negative, zero or positive.

(define c0 (λ (x1 x2) (- x1 x2)))
(define c1 (λ (x1 x2) (- (string-length x1) (string-length x2))))
(define c2 (λ (x1 x2) (- (length x1) (length x2))))

; comp-temp : [Comparison X] -> ?
(define (comp-temp comp)
  ...)

; -------------------------
(define-struct leaf [])
(define-struct node [key info smaller bigger])
 
; A [BST X Y] is one of
; - (make-leaf)
; - (make-node X Y [BST X Y] [BST X Y])
; and represents either an empty binary search tree or
; a node with key, info, and two subtrees with smaller/larger keys, respectively

(define bst0 (make-leaf))
(define bst1 (make-node 0 "a" (make-leaf) (make-leaf)))
(define bst2 (make-node 0 "a" (make-leaf)
                        (make-node 1 "b" (make-leaf) (make-leaf))))

; bst-temp : [BST X Y] -> ?
(define (bst-temp bst)
  (cond [(leaf? bst) ...]
        [(node? bst) (... (node-key bst)
                          (node-info bst)
                          (bst-temp (node-smaller bst))
                          (bst-temp (node-bigger bst)) ...)]))

; ---------------------------------------
(define-struct treemap [comp bst])
; An [ATreeMap X Y] is a (make-treemap [Comparison X] [BST X Y])
; The bst field is a binary search tree with values of type Y, whose keys are
; of type X and that are ordered according to the comparison function in the comp field.

(define tm0 (make-treemap c0 bst0))
(define tm1 (make-treemap c0 bst1))
(define tm2 (make-treemap c0 bst2))

; tm-temp : [ATreeMap X Y] -> ?
(define (tm-temp tm)
  (... (comp-temp (treemap-comp tm))
       (bst-temp (treemap-bst tm)) ...))

; 1 ----------------------------------

; insert : {X Y} X Y [ATreeMap X Y] -> [ATreeMap X Y]
; inserts a key info pair into a treemap
; (If the key is already present, the old value should be overwritten)
(define (insert key info tm)
  (make-treemap (treemap-comp tm)
                (bst-temp (treemap-bst tm))))

; insert-bst : [BST X Y] [Comparison X] -> [BST X Y]
; inserts a key info pair into a BST in the order according to the comparison function
(define (insert-bst bst comp)
  (cond [(leaf? bst) ...]
        [(node? bst) (... (node-key bst)
                          (node-info bst)
                          (bst-temp (node-smaller bst))
                          (bst-temp (node-bigger bst)) ...)]))

