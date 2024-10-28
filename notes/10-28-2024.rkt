#lang htdp/isl+

; 10/28/2024

; [Set-of [List-of Name [Set-of Name]]]


; Graph
; - Nodes
; - Edges
;   - directed
;   - undirected

(define graph-1
  (list (list 'Amy (list 'Bill 'Claire))
        (list 'Ben '())
        (list 'Bill (list 'Ben))
        (list 'Claire (list 'Eli))
        (list 'Eli (list 'Claire))))

(define graph-2
  '((Amy (Bill Claire))
    (Bill (Ben))
    (Claire (Eli))
    (Ben (Daniel))
    (Eli (Claire))))


(check-expect (good? graph-1) #t)
(check-expect (good? graph-2) #f)

; Graph -> Boolean
(define (good? g)
  (local [; (list Symbol [List-of Symbol]) -> Boolean
          (define (has-good-neighbors? node-and-edges)
            (local [; (define user (first node-and-edges))
                    (define friends (second node-and-edges))]
              (andmap (λ(friend) (contains-node? g friend)) friends)
              ))]
  (andmap has-good-neighbors? g)))

(define (contains-node? g name)
  (ormap (λ(node-and-edges) (symbol=? name (first node-and-edges))) g))


(define (good2? g)
  (local [(define node-names (map first g))]
    (andmap (λ(node-and-edges) (andmap (λ(edge) (good-node2? edge node-names)) (second node-and-edges))) g)))

(define (good-node2? n node-names)
  (member? n node-names))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; path? : Graph Name Name -> Boolean
; is there a path in the graph from node1 to node2
(check-expect (path? graph-1 'Amy 'Claire) #t)
(check-expect (path? graph-1 'Amy 'Ben) #t)
(check-expect (path? graph-1 'Claire 'Ben) #f)

(define (path1 g a b)
  (local [(define a-friends (lookup-friends g a))]
    (or (member? b a-friends)
        (ormap (λ(friend-of-a) (path1 g friend-of-a b)) a-friends))))

; Graph Name -> [List-of Name]
; assume given name is in the graph
(define (lookup-friends g name)
  (cond [(empty? g) (error name "not found")]
        [(cons g)
         (local [(define first-node (first g))
                 (define rest-graph (rest g))]
         (if (symbol=? name (first first-node))
             (second first-node)
             (lookup-friends rest-graph name)))]))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; accumulator
; four questions
; what does accumulator describe?
; what is the initial accumulator
; how does the func update teh accumulator as it recurs?
; how can teh func exploit the ifo in the accumulator

; purpose + accumulator 4 questions
; termination statement - why our function always terminates