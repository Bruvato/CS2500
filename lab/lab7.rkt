#lang htdp/isl+

; A [BinTree X] is one of
; - (make-leaf X)
; - (make-node [BinTree X] [BinTree X])
; It represents a (full) binary tree with values (of type X)
; at the leaves of the tree
(define-struct leaf [val])
(define-struct node [left right])

(define (bintreetemp bintree)
  (cond [(leaf? bintree) (leaf-val bintree)]
        [(node? bintree) (... (bintreetemp (node-left bintree))
                              (bintreetemp (node-right bintree)) ...)]))

(define bt1 (make-leaf 0))
(define bt2 (make-node (make-leaf 1) (make-leaf 2)))
(define bt3 (make-node bt2 bt1))
(define bt4 (make-node bt1 (make-node (make-leaf 4) (make-leaf 6))))


; tree-flatten : [BinTree X] -> [List-of X]
; converts a tree into a list
(check-expect (tree-flatten bt1) (list 0))
(check-expect (tree-flatten bt2) (list 1 2))
(check-expect (tree-flatten bt3) (list 1 2 0))

#;(define (tree-flatten bintree)
  (cond [(leaf? bintree) (list (leaf-val bintree))]
        [(node? bintree) (append (tree-flatten (node-left bintree))
                                 (tree-flatten (node-right bintree)))]))


; height : {X} [BinTree X] -> Natural
; Determines the height of the given tree
; This is the length of the longest path from the root to a leaf
(define (height bt)
  (cond [(leaf? bt) 0]
        [(node? bt) (add1 (max (height (node-left bt))
                               (height (node-right bt))))]))

; tree-reverse : [BinTree X] -> [BinTree X]
; reverses a given tree
(check-expect (tree-reverse bt1) bt1)
(check-expect (tree-reverse bt2) (make-node (make-leaf 2) (make-leaf 1)))
(check-expect (tree-reverse bt3) (make-node (make-leaf 0) (make-node (make-leaf 2) (make-leaf 1))))

(define (tree-reverse bintree)
  (cond [(leaf? bintree) bintree]
        [(node? bintree) (make-node (tree-reverse (node-right bintree))
                                    (tree-reverse (node-left bintree)))]))

; tree-map : [BinTree X] [X -> X] -> [BinTree X]
; maps a function to each element of the given binary tree
(check-expect (tree-map bt2 add1) (make-node (make-leaf 2) (make-leaf 3)))
(check-expect (tree-map bt3 add1) (make-node (make-node (make-leaf 2) (make-leaf 3)) (make-leaf 1)))
(define (tree-map bintree fn)
  (cond [(leaf? bintree) (make-leaf (fn (leaf-val bintree)))]
        [(node? bintree) (make-node (tree-map (node-left bintree) fn)
                                    (tree-map (node-right bintree) fn))]))

; tree-andmap : [BinTree X] [X -> Boolean] -> Boolean
; checks if all values in the tree satisfy the predicate
(check-expect (tree-andmap bt1 zero?) #true)
(check-expect (tree-andmap bt2 even?) #false)
(check-expect (tree-andmap bt4 even?) #true)

(define (tree-andmap bintree p?)
  (cond [(leaf? bintree) (p? (leaf-val bintree))]
        [(node? bintree) (and (tree-andmap (node-left bintree) p?)
                              (tree-andmap (node-right bintree) p?))]))



; tree-ormap : [BinTree X] [X -> Boolean] -> Boolean
; checks if at least 1 of the values in the tree satisfy the predicate
(check-expect (tree-ormap bt1 zero?) #true)
(check-expect (tree-ormap bt2 negative?) #false)
(check-expect (tree-ormap bt4 even?) #true)

(define (tree-ormap bintree p?)
  (cond [(leaf? bintree) (p? (leaf-val bintree))]
        [(node? bintree) (or (tree-ormap (node-left bintree) p?)
                             (tree-ormap (node-right bintree) p?))]))

; tree-filter : [BinTree X] [X -> Boolean] X -> [BinTree X]
; filters the values in the given tree that satisfy the given predicate, replaces the
; values that do not satisfy the predicate with the given placeholder value
(check-expect (tree-filter bt2 even? -1) (make-node (make-leaf -1) (make-leaf 2)))

(define (tree-filter bintree p? place-holder)
  (cond [(leaf? bintree) (if (p? (leaf-val bintree)) bintree (make-leaf place-holder))]
        [(node? bintree) (make-node (tree-filter (node-left bintree) p? place-holder)
                                    (tree-filter (node-right bintree) p? place-holder))]))

; tree-fold : {X,Y} [X -> Y] [Y Y -> Y] [BinTree X] ->  Y
(check-expect (tree-fold add1 + bt2) 5)
(define (tree-fold fn combine-fn bintree)
  (cond [(leaf? bintree) (fn (leaf-val bintree))]
        [(node? bintree) (combine-fn (tree-fold fn combine-fn (node-left bintree))
                                     (tree-fold fn combine-fn (node-right bintree)))]))



; even-length-total : [BinTree X] -> Natural
;
(check-expect (even-length-total (make-node (make-leaf "hi") (make-leaf "abcd"))) 6)
(check-expect (even-length-total (make-node (make-leaf "hey") (make-leaf "abcd"))) 4)

(define (even-length-total bintree)
  (cond [(leaf? bintree) (if (even? (string-length (leaf-val bintree))) (string-length (leaf-val bintree)) 0)]
        [(node? bintree) (+ (even-length-total (node-left bintree))
                            (even-length-total (node-right bintree)))]))

; tree-flatten : [BinTree X] -> [List-of X]

(define (tree-flatten bintree)
  (tree-fold identity cons bintree))
