#lang htdp/isl+

; 10/16/2024
; ==============
; more trees
; ==============

(define-struct leaf [])

(define-struct node
  [key info left right])

; A BT is one of
; -make leaf
; -mad node


(define (bt-temp bt)
  (cond [(leaf? bt) ...]
        [(node? bt)
         (... (node-key bt)
              (node-info bt)
              (bt-temp (node-left bt))
              (bt-temp (node-right bt)) ...)]))


(define L (make-leaf))
(define t (make-node 10 "a"
                     (make-node 9 "b"
                                (make-node 14 "c" L L)
                                L)
                     L))


; contains? : BT Natural -> Boolean
; is k is bt
(check-expect (contains? s 11) #t)
(check-expect (contains? t 12) #f)

(define (contains? bt k)
  (cond [(leaf? bt) #f]
        [(node? bt) (or (= k (node-key bt))
                        (contains? (node-left bt) k)
                        (contains? (node-right bt) k))]))

; assume key is present
; lookup : BT Natural -> [Maybe String]

; A [Maybe X] is one of
; - #false
; - X
(check-expect (lookup s 11) "b")
(check-expect (lookup s 7) "f")

(define (lookup bt k)
  (cond [(leaf? bt) #f]
        [(node? bt) (cond [(= k (node-key bt))          (node-info bt)]
                          [(contains? (node-left bt) k) (lookup (node-left bt) k)]
                          [else                         (lookup (node-right bt) k)])]))
                        
                        

; =====

; A Binary Search Tree (BST) is a BinaryTree such that
; in any node (make-node K _ L R), we know that
; all the keys in L are < K and all the keys in R are > K


(define (lookup-bst bst k)
  (cond [(leaf? bst) (error k "not found")]
        [(node? bst)
         (cond [(= k (node-key bst)) (node-info bst)]
               [(< k (node-key bst)) (lookup-bst (node-left bst) k)]
               [(> k (node-key bst)) (lookup-bst (node-right bst) k)])]))

; insert : Nat String BST -> BST
; assume already there and room for it
(define (insert key info bst)
  (cond [(leaf? bst) (make-node key info L L)]
        [(node? bst)
         (cond [(= key (node-key bst)) (error key "already present")]
               [(< key (node-key bst))
                (make-node (node-key bst) (node-info bst)
                           (insert key info (node-left bst))
                           (node-right bst))]
               [(> key (node-key bst))
                (make-node (node-key bst)
                           (node-info bst)
                           (node-left bst)
                           (insert key info (node-right bst)))])]))

; a Pair is a (list k v)
; define func buil-bst [List-of Pair] -> BST

(define (buid-bst keyvals)
  (foldr (λ(kv bst-so-far) (insert (first kv) (second kv) bst-so-far)) L  keyvals))
        
               
               
