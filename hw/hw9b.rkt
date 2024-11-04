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

(define bst0 (make-leaf)) ; leaf
(define bst1 (make-node 0 "a" (make-leaf) (make-leaf))) ; node
(define bst2 (make-node 0 "a"
                        (make-leaf) 
                        (make-node 1 "b" (make-leaf) (make-leaf)))) ; node leaf node
(define bst3 (make-node 0 "a"
                        (make-node -1 "c" (make-leaf) (make-leaf))
                        (make-node 1 "b" (make-leaf) (make-leaf)))) ; node node node

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


; NOTE: To test functions that return treemaps,
; you will have to extract the BST,
; since the comparison functions will not work inside check-expect.

; 1 ============================================

; insert : {X Y} X Y [ATreeMap X Y] -> [ATreeMap X Y]
; inserts a key info pair into a treemap
; (If the key is already present, the old value should be overwritten)
(define (insert key info tm)
  (make-treemap (treemap-comp tm)
                (insert-bst key info (treemap-comp tm) (treemap-bst tm))))

(check-expect (treemap-bst (insert 1 "a" (make-treemap - bst0)))
              (make-node 1 "a" (make-leaf) (make-leaf)))
(check-expect (treemap-bst (insert 1 "a" (make-treemap - bst1)))
              (make-node 0 "a"
                         (make-leaf)
                         (make-node 1 "a" (make-leaf) (make-leaf))))
(check-expect (treemap-bst (insert 0 "new" (make-treemap - bst0)))
              (make-node 0 "new" (make-leaf) (make-leaf)))

; insert-bst : {X Y} X Y [Comparison X] [BST X Y] -> [BST X Y]
; inserts a key info pair into a BST in the order according to the comparison function
(define (insert-bst key info comp bst)
  (cond [(leaf? bst) (make-node key info (make-leaf) (make-leaf))]
        [(node? bst) (local [(define smaller (insert-bst key info comp (node-smaller bst)))
                             (define bigger (insert-bst key info comp (node-bigger bst)))]
                       (if (negative? (comp key (node-key bst)))
                           (make-node (node-key bst) (node-info bst) smaller (node-bigger bst))
                           (if (positive? (comp key (node-key bst)))
                               (make-node (node-key bst) (node-info bst) (node-smaller bst) bigger)
                               (make-node key info (node-smaller bst) (node-bigger bst)))))]))

(check-expect (insert-bst 1 "a" - (make-leaf))
              (make-node 1 "a" (make-leaf) (make-leaf)))
(check-expect (insert-bst 1 "a" - (make-node 0 "b" (make-leaf) (make-leaf)))
              (make-node 0 "b" (make-leaf) (make-node 1 "a" (make-leaf) (make-leaf))))
(check-expect (insert-bst -1 "a" - (make-node 0 "b" (make-leaf) (make-leaf)))
              (make-node 0 "b" (make-node -1 "a" (make-leaf) (make-leaf)) (make-leaf)))
(check-expect (insert-bst 0 "a" - (make-node 0 "b" (make-leaf) (make-leaf)))
              (make-node 0 "a" (make-leaf) (make-leaf)))

; 2 ==========================================================

; find : {X Y} [ATreeMap X Y] X -> Y
; finds the associated info in ATreeMap given a key
; (error if no such key is found)
(define (find tm key)
  (find-bst key (treemap-comp tm) (treemap-bst tm)))

(check-error (find (make-treemap - bst0) 0))
(check-error (find (make-treemap - bst1) 1))
(check-expect (find (make-treemap - bst1) 0) "a")
(check-expect (find (make-treemap - bst2) 1) "b")

; find-bst {X Y} X [Comparison X] [BST X Y] -> Y
; finds the associated info in a BST given a key and the comparison function
(define (find-bst key comp bst)
  (cond [(leaf? bst) (error "No such key found")]
        [(node? bst) (if (negative? (comp key (node-key bst)))
                         (find-bst key comp (node-smaller bst))
                         (if (positive? (comp key (node-key bst)))
                             (find-bst key comp (node-bigger bst))
                             (node-info bst)))]))

(check-error (find-bst 0 - (make-leaf)))
(check-error (find-bst 0 - (make-node 1 "a" (make-leaf) (make-leaf))))
(check-expect (find-bst 1 - (make-node 1 "a" (make-leaf) (make-leaf))) "a")
(check-expect (find-bst 1 - (make-node 0 "a" (make-leaf)
                                       (make-node 1 "b" (make-leaf) (make-leaf)))) "b")

; 3 ==========================================

; submap : [ATreeMap X Y] X X -> [ATreeMap X Y]
; outputs a treemap where the binary search tree is a new tree
; containing all the keys (and associated data) from the original tree
; that are at least lo and at most hi (as indicated by the comparison function).
(define (submap tm lo hi)
  (make-treemap (treemap-comp tm)
                (submap-bst lo hi (treemap-comp tm) (treemap-bst tm))))

(check-expect (treemap-bst (submap (make-treemap - bst0) 0 1)) (make-leaf))
(check-expect (treemap-bst (submap (make-treemap - bst1) 0 1)) bst1)
(check-expect (treemap-bst (submap (make-treemap - bst1) -1 0)) bst1)
(check-expect (treemap-bst (submap (make-treemap - bst2) 0 1)) bst2)
(check-expect (treemap-bst (submap (make-treemap - bst2) -1 0))
              (make-node 0 "a" (make-leaf) (make-leaf)))
(check-expect (treemap-bst (submap (make-treemap - bst3) 0 1))
              (make-node 0 "a"
                         (make-leaf)
                         (make-node 1 "b" (make-leaf) (make-leaf))))
(check-expect (treemap-bst (submap (make-treemap - bst3) 0 0))
              (make-node 0 "a" (make-leaf) (make-leaf)))

; submap-bst : X X [Comparison X] [BST X Y] -> [BST X Y]
; produces a new BST containing all the keys (and associated data) from the original tree
; that are at least lo and at most hi (as indicated by the comparison function).
(define (submap-bst lo hi comp bst)
  (cond [(leaf? bst) (make-leaf)]
        [(node? bst) (local [(define smaller (submap-bst lo hi comp (node-smaller bst)))
                             (define bigger (submap-bst lo hi comp (node-bigger bst)))]
                       (cond [(negative? (comp (node-key bst) lo)) ; too low
                              bigger]
                             [(positive? (comp (node-key bst) hi)) ; too high
                              smaller]
                             [else (make-node (node-key bst) (node-info bst)
                                              smaller
                                              bigger)]))]))

(check-expect (submap-bst 0 1 - (make-leaf)) (make-leaf))
(check-expect (submap-bst 0 1 - (make-node 1 "a" (make-leaf) (make-leaf)))
              (make-node 1 "a" (make-leaf) (make-leaf)))
(check-expect (submap-bst -1 0 - (make-node 1 "a" (make-leaf) (make-leaf)))
              (make-leaf))
(check-expect (submap-bst 0 1 - (make-node 1 "a"
                                           (make-node 0 "b" (make-leaf) (make-leaf))
                                           (make-node 2 "c" (make-leaf) (make-leaf))))
              (make-node 1 "a"
                         (make-node 0 "b" (make-leaf) (make-leaf))
                         (make-leaf)))
(check-expect (submap-bst 1 2 - (make-node 1 "a"
                                           (make-node 0 "b" (make-leaf) (make-leaf))
                                           (make-node 2 "c" (make-leaf) (make-leaf))))
              (make-node 1 "a"
                         (make-leaf)
                         (make-node 2 "c" (make-leaf) (make-leaf))))
(check-expect (submap-bst 1 1 - (make-node 1 "a"
                                           (make-node 0 "b" (make-leaf) (make-leaf))
                                           (make-node 2 "c" (make-leaf) (make-leaf))))
              (make-node 1 "a"
                         (make-leaf)
                         (make-leaf)))

(check-expect (submap-bst -1 1 - (make-node 0 "a"
                                            (make-node -2 "b"
                                                       (make-leaf)
                                                       (make-node -1 "c" (make-leaf) (make-leaf)))
                                            (make-leaf)))
              (make-node 0 "a"
                         (make-node -1 "c" (make-leaf) (make-leaf))
                         (make-leaf)))



