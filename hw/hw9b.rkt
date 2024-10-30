#lang htdp/isl+

; A [Comparison X] is a [X X -> Number]
; Interpretation: A Comparison function allows us to compare two inputs,
; with: a negative output indicating the first input is less than the second input,
;       a zero output     indicating the two inputs are equal,
;       a positive output indicating the first input is greater than the second input,
; according to some ordering.  The exact numerical values don't matter, but
; only whether they are negative, zero or positive.
 
(define-struct leaf [])
(define-struct node [key info smaller bigger])
 
; A [BST X Y] is one of
; - (make-leaf)
; - (make-node X Y [BST X Y] [BST X Y])
; and represents either an empty binary search tree or
; a node with key, info, and two subtrees with smaller/larger keys, respectively
 
(define-struct treemap [comp bst])
; An [ATreeMap X Y] is a (make-treemap [Comparison X] [BST X Y])
; The bst field is a binary search tree with values of type Y, whose keys are
; of type X and that are ordered according to the comparison function in the comp field.

