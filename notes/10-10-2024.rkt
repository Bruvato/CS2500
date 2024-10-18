#lang htdp/isl+

; 10/10/2024
; ============
; cart prod & TREES
; ============


(define-struct pair [fst snd])

; {S} [List-of [List-of S]] -> [List-of S]
(define (flatten lol)
  ; foldr : {A B} [A B -> B] B [List-of A] -> B
  ; we want B = [List-of S]
  ; we have [List-of A] = [List-of [List-of S]]
  ; so A = [List-of S]
  ; so we need helper with signature : [List-of S] [List-of S] -> [List-of S]
  (foldr append '() lol))

; [Set-of X] [Set-of Y] -> [Set-of [Pair-of X Y]]
(define (cartesian-prod setx sety)
  (flatten
   (local [; X -> [List-of [Pair-of X Y]]
           (define (process-x elemx)
             (local [; for each val x in set x, for each val y in set y, create a pair (x,y)
                     ; {A B} [A -> B] [List-of A] -> [List-of B]
                     ; A = Y
                     ; B = [Pair-of X Y]
                     ; we want a [List-of [Pair-of X Y]]
                     (define (add-x-to-each-y elemy)
                       (make-pair elemx elemy))]
               (map add-x-to-each-y sety)))]
     (map process-x setx))))

; we want [List-of [List-of stuff]] > [List-of stuff]

(define (cart-prod setx sety)
  (flatten
   (map (λ(x) (map (λ(y) (make-pair x y)) sety)) setx)))


; =====================
; TREES
; =====


; A List of Strings is
; - '()
; - (cons String LOS)

(define (los-temp los)
  (cond
    [(empty? los) ...]
    [(cons? los) (... (first los)
                      (los-temp (rest los)) ...)]))



(define-struct limb [length rest])

(define-struct branch [left right])

; A FruitTree is one of
; - Number
; - (make-limb Number FruitTree)
; - (make-branch FruitTree FruitTree)
; INTERP
; - rep weight of fruit in oz
; - or length of the limb in ft
; - or the junction of two branches of a tree

(define (ft-temp ft)
  (cond
    [(number? ft) ...]
    [(limb? ft) (... (limb-length ft)
                     ... (ft-temp (limb-rest ft)) ...)]
    [(branch? ft) (... (ft-temp (branch-left ft))
                       ... (ft-temp (brahcn-right ft)) ...)]))

; EXAMPLES
(define FT1 4)
(define FT2 (make-limb 1 2))
(define FT3 (make-branch (make-limb 2 3) 4))
(define FT4 (make-branch FT2 (make-branch FT3 FT1)))

; count-fruit : FruitTree -> Nat
(check-expect (count-fruit FT1) 1)
(check-expect (count-fruit FT2) 1)
(check-expect (count-fruit FT3) 2)
(check-expect (count-fruit FT4) 4)

(define (count-fruit ft)
  (cond
    [(number? ft) 1]
    [(limb? ft)
     (count-fruit (limb-rest ft))]
    [(branch? ft)
     (+ (count-fruit (branch-left ft))
        (count-fruit (branch-right ft)))]))

; fruit-weight : FruitTree -> Nat
(check-expect (fruit-weight FT1) 4)
(check-expect (fruit-weight FT2) 2)
(check-expect (fruit-weight FT3) (+ 3 4))
(check-expect (fruit-weight FT4) (+ 2 3 4 4))

(define (fruit-weight ft)
   (cond
    [(number? ft) ft]
    [(limb? ft)
     (fruit-weight (limb-rest ft))]
    [(branch? ft)
     (+ (fruit-weight (branch-left ft))
        (fruit-weight (branch-right ft)))]))


; a BinaryTree is one of
; - (make-leaf) = '()
; - (make-node Number BinaryTree BinaryTree)
(define-struct leaf [])
(define-struct node [val left right])

(define (bt-temp bt)
  (cond
    [(leaf? bt) ...]
    [(node? bt)
     (... (node-val bt)
          (bt-temp (node-left bt))
          (bt-temp (node-right b)))]))

(define L (make-leaf))
(define TREE6
  (make-node 6
             (make-node 3
                        L
                        (make-node 2
                                   L
                                   (make-node 1 L L)))
             (make-node 5
                        (make-node 4 L L)
                        L)))

(define (height bt)
  (cond
    [(leaf? bt) 0]
    [(node? bt)
     (add1 (max (height (node-left bt))
                (height (node-right bt))))]))

(define (longest-path bt)
  (cond
    [(leaf? bt) 0]
    [(node? bt)
     (+ (node-val bt)
        (max (longest-path (node-left bt))
             (longest-path (node-right bt))))]))






