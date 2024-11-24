#lang htdp/isl+

; easy, meidum, tricky, slightly tricky (extra credit)


; ---
; A Nat is one of
; - 0
; (add1 Nat)

; dont need termin statement
; termin statment is contained in the data def

; euclids requires two base cases: zero? one?

; ---
; list of bases temp not required
(define (rr-temp rr)
  (... (list-temp (rr-bases rr))
       ...
       (recursive-temp (rr-recurrence rr))))

(define (recursive-temp r)
  (... ((recursive-gen-args r) ... )
       ((recursive-combine-results r) ... ...)))

; ---
; list abstractions / helpers given
; cheat sheet

; ---
; cross product template
; A BT (BinTree) is one of
; - 'leaf
(define-struct node [left val right])
; - (make-node BT Number BT)

; 2 clauses so 2x2=4 min tests

; t1\t2 | leaf | node
; ---------------------
; leaf  |      |
; ---------------------
; node  |      |



(define (tree=? t1 t2)
  (cond [(and (symbol? t1) (symbol? t2)) #t]
        [(and (symbol? t1) (node? t2)) #f]
        [(and (node? t1) (symbol? t2)) #f]
        [(and (node? t1) (node? t2)) (and (= (node-val t1) (node-val t2))
                                          (tree=? (node-left t1) (node-left t2))
                                          (tree=? (node-right t1) (node-right t2)))]))

; ---
; generative recursion
; - lists
; - graphs (delete the ones we already visited)
; bundle (accumulation)

; pass by info that we need later on -> accum
; 

(define (tree-flatten t)
  (local [(define (flat/acc t flattened-right)
            ; flattened-right is a list of everything to my right in original tree
            ; starts at '()
            ; grows with data in node
            ; return this as the final result
            (cond [(symbol? t) flattened-right]
                  [(node? t)
                   (flat/acc (node-left t)
                             (cons (node-val t)
                                   (flat/acc (node-right t) flattened-right)))]))]
    (flat/acc t '())))




; A NumBinTree is one of
; 'leaf
; (make-node Num NumBinTree NumBinTree)
;(define-struct node [val left right])

#|
(define (flatten-tree bt)
  (local [(define (flatten-tree/acc bt acc)
            (cond [(symbol? bt) ...]
                  [(node? bt) (... (node-val bt)
                                   (node-left bt)
                                   (node-right bt
            
            ...)]
    (flatten-tree/acc bt '()))
|#




; -------------------------------------


(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 

; Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(define (find-path origination destination G)
  

(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)











