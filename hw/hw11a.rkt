#lang htdp/isl+

(define-struct graph [nodes neighbors])
; A Graph is a (make-graph [Set-of Symbol] [Symbol -> [Set-of Symbol]))
; and represents the nodes and edges in a graph.

; A [Set-of X] is a [List-of X], but where we don't care about the
; order of the elements.

; All symbols that represent nodes are assumed to be unique (i.e. a set)
; All symbols returned by (graph-neighbors _) will also be unique (again, a set)
; All symbols returned by (graph-neighbors _) are assumed to be in nodes


(define s0 '())
(define s1 '(1 2 3))
(define s2 '(a b c))
(define s3 (list #t #f))

; set-temp : {X} [Set-of X] -> ?
(define (set-temp s)
  (cond [(empty? s) ...]
        [(cons? s) (... (first s)
                        (set-temp (rest s)) ...)]))

(define g-ex (make-graph
              '(anne billie carl dave) ; the names of the nodes
              (lambda (name) ; a function computing the neighbors of each node
                (cond [(symbol=? name 'anne)   '(billie carl)]
                      [(symbol=? name 'billie) '(anne)]
                      [(symbol=? name 'carl)   '(anne dave)]
                      [(symbol=? name 'dave)   '(carl)]))))

; graph-temp : Graph -> ?
(define (graph-temp g)
  (... (set-temp (graph-nodes g))
       (graph-neighbors g) ...))

; ======================GRAPH EXAMPLES==============================

; no nodes
(define g0 (make-graph '() (λ (s) '())))

;1 isolated node
(define g1i (make-graph '(a) (λ (s) '())))

; 1 self-loop node
(define g1s (make-graph '(a) (λ (s) '(a))))

; 2 isolated nodes
(define g2i (make-graph '(a b) (λ (s) '())))

; 2 connected nodes (one way)
(define ga->b (make-graph '(a b)
                          (λ (s)
                            (cond [(symbol=? s 'a) '(b)]
                                  [(symbol=? s 'b) '()]))))

; 2 connected nodes (one way)
(define ga<-b (make-graph '(a b)
                          (λ (s)
                            (cond [(symbol=? s 'a) '()]
                                  [(symbol=? s 'b) '(a)]))))

; 2 connected nodes (bidirectional edge)
(define ga-b (make-graph '(a b)
                         (λ (s)
                           (cond [(symbol=? s 'a) '(b)]
                                 [(symbol=? s 'b) '(a)]))))

; 2 self-loop connected nodes (bidirectional edge)
(define ga-bs (make-graph '(a b)
                          (λ (s)
                            (cond [(symbol=? s 'a) '(a b)]
                                  [(symbol=? s 'b) '(b a)]))))

; 3 isolated nodes
(define g3i (make-graph '(a b c) (λ (s) '())))

; 3 connected nodes (bidirectional edges)
(define g3c (make-graph '(a b c)
                        (λ (s)
                          (cond [(symbol=? s 'a) '(b c)]
                                [(symbol=? s 'b) '(a c)]
                                [(symbol=? s 'c) '(a b)]))))

; 3 self-loop connected nodes (bidirectional edges)
(define g3cs (make-graph '(a b c)
                         (λ (s)
                           (cond [(symbol=? s 'a) '(a b c)]
                                 [(symbol=? s 'b) '(a b c)]
                                 [(symbol=? s 'c) '(a b c)]))))

; 3 linked nodes (bidrectional edges)
(define ga-b-c (make-graph '(a b c)
                           (λ (s)
                             (cond [(symbol=? s 'a) '(b)]
                                   [(symbol=? s 'b) '(a c)]
                                   [(symbol=? s 'c) '(b)]))))

; 4 nodes pattern (bidrectional)
(define g4 (make-graph '(a b c d)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(b c)]
                               [(symbol=? s 'b) '(a)]
                               [(symbol=? s 'c) '(a d)]
                               [(symbol=? s 'd) '(c)]))))

; conneced square nodes
(define gsquare (make-graph '(a b c d)
                            (λ (s)
                              (cond [(symbol=? s 'a) '(b c)]
                                    [(symbol=? s 'b) '(a d)]
                                    [(symbol=? s 'c) '(a d)]
                                    [(symbol=? s 'd) '(b c)]))))

; 5 node pattern
(define g5 (make-graph '(a b c d e)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(b c)]
                               [(symbol=? s 'b) '(d)]
                               [(symbol=? s 'c) '(a e)]
                               [(symbol=? s 'd) '()]
                               [(symbol=? s 'e) '(e)]))))

; 6 nodes
(define g6 (make-graph '(a b c d e f)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(c d e f)]
                               [(symbol=? s 'b) '(c d e f)]
                               [(symbol=? s 'c) '(a b)]
                               [(symbol=? s 'd) '(a b)]
                               [(symbol=? s 'e) '(a b)]
                               [(symbol=? s 'f) '(a b)]))))

; =============================10A HELPERS======================================

; neighbor-of? : Graph Symbol Symbol -> Boolean
; determines if the second symbol is a neighbor of the first one in a Graph
; (Assume both symbols are in the graph)
(define (neighbor-of? g s1 s2)
  (element-of? s2 ((graph-neighbors g) s1) symbol=?))

; element-of? : {X} X [Set-of X] [X X -> Boolean]
; determines if X is an element of a Set-of X
(define (element-of? x s e?)
  (ormap (λ (elem) (e? x elem)) s))

; remove-duplicates : [List-of X] [X X -> Boolean] -> [List-of X]
; removes the duplicates of a given list
(define (remove-duplicates l e?)
  (foldr (λ (elem acc) (if (element-of? elem acc e?) acc (cons elem acc))) '() l))

; set-equal? : [Set-of X] [Set-of X] [X X -> Boolean] -> Boolean
; determines if two sets are equal
(define (set-equal? s1 s2 e?)
  (if (or (has-duplicates? s1 e?) (has-duplicates? s2 e?))
      (error "set-equal?: input list contains duplicates")
      (and (subset? s1 s2 e?)
           (subset? s2 s1 e?))))

; has-duplicates? : [List-of X] [X X -> Boolean] -> Boolean
; determines if a list contains any duplicates
(define (has-duplicates? l e?)
  (not (= (length l) (length (remove-duplicates l e?)))))

; subset? : [Set-of X] [Set-of X] [X X -> Boolean] -> Boolean
; determines if the first set is a subset of the second set
(define (subset? s1 s2 e?)
  (andmap (λ (elem1) (element-of? elem1 s2 e?)) s1))

; graph=? : Graph Graph -> Boolean
; determines if two graphs are equal
; (two graphs are the same if they have the same set of nodes)
(define (graph=? g1 g2)
  (set-equal? (graph-nodes g1) (graph-nodes g2)
              (λ (node1 node2) (node=? node1 node2 (graph-neighbors g1) (graph-neighbors g2)))))

; node=? : [Set-of Symbol] [Set-of Symbol] [Symbol -> [Set-of Symbol]] [Symbol -> [Set-of Symbol]]
; determines if two nodes are the same
; (they are represented by the same symbol
; and have the same set of neighbors)
(define (node=? node1 node2 get-neighbors1 get-neighbors2)
  (and (symbol=? node1 node2)
       (set-equal? (get-neighbors1 node1) (get-neighbors2 node2) symbol=?)))


; reverse-edges : Graph -> Graph
; reverses the edges of a given graph
(define (reverse-edges g)
  (make-graph (graph-nodes g) (λ (name)
                                (filter (λ (node) (neighbor-of? g node name)) (graph-nodes g)))))

; rename : Graph [List-of Symbol] -> Graph
; renames the nodes in the graph to the names in the list of symbols
(define (rename g los)
  (make-graph
   los
   (rename-neighbors los (graph-nodes g) (graph-neighbors g))))

; rename-neighbors : [List-of Symbol] [Set-of Symbol] [Symbol -> [Set-of Symbol]]
; produces a function that returns the renamed neighbhors given a renamed node
(define (rename-neighbors los nodes get-neighbors)
  (λ (name) (map (λ (node) (lookup node nodes los)) (get-neighbors (lookup name los nodes)))))
              

; lookup : Symbol [List-of Symbol] [List-of Symbol] -> Symbol
; maps the 1st elem of the 1st list to the 1st elem of the 2nd list
; maps the 2nd elem of the 1st list to the 2nd elem of the 2nd list and so on
; (two given lists are equal length)
(define (lookup s los1 los2)
  (cond [(empty? los1) (error "elem not in los")]
        [(cons? los1) (if (symbol=? s (first los1))
                          (first los2)
                          (lookup s (rest los1) (rest los2)))]))


; =============================================

;(require 2htdp/abstraction)


; graph=?/curried : Graph -> [Graph -> Boolean]
; Curried graph=?
(define graph=?/curried (λ (g1) (λ (g2) (graph=? g1 g2))))
 
; f : Graph ... -> Graph
; Do something to g
#;(define (f g ...) ...)
 
#;(check-satisfied (f some-input-graph ...)
                   (graph=?/curried
                    some-expected-graph))

; 1 =======================================================

(define swap-in0 (make-graph '(a b c)
                             (λ (n) (cond [(symbol=? n 'a) '(b c)]
                                          [(symbol=? n 'b) '(b)]
                                          [(symbol=? n 'c) '(a)]))))

(define swap-out0 (make-graph '(0->1 0->2 1->1 2->0)
                              (λ (n) (cond [(symbol=? n '0->1) '(1->1)]
                                           [(symbol=? n '0->2) '(2->0)]
                                           [(symbol=? n '1->1) '(1->1)]
                                           [(symbol=? n '2->0) '(0->1 0->2)]))))

(define swap-in1 (make-graph '(a b)
                             (λ (n) (cond [(symbol=? n 'a) '(b)]
                                          [(symbol=? n 'b) '(a)]))))

(define swap-out1 (make-graph '(0->1 1->0)
                              (λ (n) (cond [(symbol=? n '0->1) '(1->0)]
                                           [(symbol=? n '1->0) '(0->1)]))))

(define swap-in2 (make-graph '(a)
                             (λ (n) (cond [(symbol=? n 'a) '()]))))

(define swap-out2 (make-graph '()
                              (λ (n) '())))

(require racket/string)
; node-name->numbers : Symbol -> (list Nat Nat)
; Convert a symbol of the form 'n1->n2 to (list n1 n2)
(define (node-name->numbers s)
  (map string->number (string-split (symbol->string s) "->")))

(check-expect (node-name->numbers '0->3) '(0 3))



; swap : Graph -> Graph
; "swaps” a graph’s nodes with its edges
(define (swap g)
  (local [(define renamed-graph
            (rename g (build-list (length (graph-nodes g))
                                  (λ (n) (string->symbol (number->string n))))))
          (define new-nodes
            (swap-nodes (graph-nodes renamed-graph) (graph-neighbors renamed-graph)))
          (define new-neighbors
            (swap-neighbors new-nodes))]
    (make-graph new-nodes new-neighbors)))

(check-satisfied (swap swap-in0)
                 (graph=?/curried
                  swap-out0))

(check-satisfied (swap swap-in1)
                 (graph=?/curried
                  swap-out1))
                       
(check-satisfied (swap swap-in2)
                 (graph=?/curried
                  swap-out2))


; swap-nodes : [Set-of Symbol] -> [Set-of Symbol]
; produces the new set of nodes such that if the n1th node points to the n2th node in the graph,
; the new graph should have a node with a name of the form 'n1->n2
(define (swap-nodes nodes get-neighbors)
  (cond [(empty? nodes) '()]
        [(cons? nodes) (append
                        (append-s-to-each-elem (first nodes) (get-neighbors (first nodes)))
                        (swap-nodes (rest nodes) get-neighbors))]))

(check-expect (swap-nodes '() (graph-neighbors g0)) '())
(check-expect (swap-nodes '(a b) (graph-neighbors ga-b)) '(a->b b->a))
(check-expect (swap-nodes '(a b c) (graph-neighbors g3c)) '(a->b a->c b->a b->c c->a c->b))

; append-s-to-elem : Symbol [Set-of Symbol] -> [Set-of Symbol]
; appends a given symbol to the front of each elem in a given Set-of Symbol with "->" in between
(define (append-s-to-each-elem s sos)
  (cond [(empty? sos) '()]
        [(cons? sos) (cons
                      (string->symbol
                       (string-append (symbol->string s) "->" (symbol->string (first sos))))
                      (append-s-to-each-elem s (rest sos)))]))

(check-expect (append-s-to-each-elem 'o '()) '())
(check-expect (append-s-to-each-elem 'o '(a b)) '(o->a o->b))
(check-expect (append-s-to-each-elem 'o '(a b c)) '(o->a o->b o->c))

; swap-neighors : [Set-of Symbol] -> [Symbol -> [Set-of Symbol]]
; produces a function that returns the new set of neighbors given a symbol such that
; If two nodes 'n1->n2 and 'n3->n4 exist in the new graph,
; then 'n1->n2 has an edge pointing to 'n3->n4 if and only if n2 equals n3
(define (swap-neighbors sos)
  (λ (s) 
    (local [(define lolon (map node-name->numbers sos))
            (define lon (node-name->numbers s))
            ; swap-neighbors-helper : [List-of Natural] [List-of [List-of Natural]]
            ; -> [Set-of Symbol]
            ; produces the set of neighbors of the "swapped" node 
            (define (swap-neighbors-helper lon lolon sos)
              (cond [(empty? lolon) '()]
                    [(cons? lolon)
                     (if (= (second lon) (first (first lolon)))
                         (cons (first sos) (swap-neighbors-helper lon (rest lolon) (rest sos)))
                         (swap-neighbors-helper lon (rest lolon) (rest sos)))]))]
      (swap-neighbors-helper lon lolon sos))))

(check-expect ((swap-neighbors '(0->1 1->0)) '0->1) '(1->0))
(check-expect ((swap-neighbors '(0->1)) '0->1) '())
(check-expect ((swap-neighbors '(0->1 1->2 1->3)) '0->1) '(1->2 1->3))


; 2 ==================================================

(define G1
  (make-graph '(A B C D E F G)
              (λ (n)
                (cond [(symbol=? n 'A) '(B E)]
                      [(symbol=? n 'B) '(E F)]
                      [(symbol=? n 'C) '(D)]
                      [(symbol=? n 'D) '()]
                      [(symbol=? n 'E) '(C F A)]
                      [(symbol=? n 'F) '(D G)]
                      [(symbol=? n 'G) '()]))))


; close? : Graph Symbol Symbol Natural -> Boolean
; determines if the second node is within a given number of steps of the first node
(define (close? g s1 s2 n)
  (if (negative? n)
      (error "invalid" n)
      (within-n-steps? s1 s2 n
                       (graph-neighbors g))))

(check-expect (close? g1i 'a 'a 0) #t)
(check-expect (close? g1s 'a 'a 0) #t)
(check-expect (close? g2i 'a 'b 1) #f)
(check-expect (close? ga->b 'a 'b 1) #t)
(check-expect (close? ga->b 'b 'a 1) #f)
(check-expect (close? ga<-b 'a 'b 2) #f)
(check-expect (close? ga-b 'a 'b 0) #f)
(check-expect (close? ga-bs 'a 'b 1) #t)
(check-expect (close? ga-bs 'a 'b 0) #f)
(check-expect (close? ga-bs 'a 'a 0) #t)
(check-expect (close? g3i 'a 'b 1) #f)
(check-expect (close? g3c 'a 'b 1) #t)
(check-expect (close? g3c 'a 'b 2) #t)
(check-expect (close? g3cs 'a 'b 1) #t)
(check-expect (close? ga-b-c 'a 'c 2) #t)
(check-expect (close? g4 'a 'd 1) #f)
(check-expect (close? g4 'a 'd 2) #t)
(check-expect (close? gsquare 'a 'd 2) #t)                 
(check-expect (close? gsquare 'a 'f 10) #f)
(check-expect (close? g5 'a 'e 2) #t)
(check-expect (close? g5 'a 'e 1) #f)
(check-error (close? g5 'a 'e -1))

; within-n-steps? : Symbol Symbol Natural [Symbol -> [Set-of Symbol]]
; determines if two given nodes are within n steps of each other
(define (within-n-steps? s1 s2 n get-neighbors)
  (local [; within-n-steps?/acc :
          ; Symbol Symbol Natural [Symbol -> [Set-of Symbol]] [Set-of Symbol] [Set-of Symbol]
          ; -> Boolean
          ; determines if two given nodes are within n steps of each other
          ; ACCUM: 1. represents the current set of nodes we're checking
          ; and the nodes already visited so far
          (define (within-n-steps?/acc s1 s2 n get-neighbors current already-visited)
            ; 4. return true or false based on if we run out of steps
            ; or if we find the node in the current set of nodes we're checking
            (cond [(negative? n) #f]
                  [(symbol=? s1 s2) #t]
                  [(element-of? s2 current symbol=?) #t]
                  [else
                   (within-n-steps?/acc s1 s2 (sub1 n) get-neighbors
                                        (filter (λ (elem)
                                                  (not (element-of? elem already-visited symbol=?)))
                                                (remove-duplicates
                                                 (foldr
                                                  (λ (s acc)
                                                    ; 3. the new current is the
                                                    ; new layer of neighbors
                                                    (append (get-neighbors s) acc)) '() current)
                                                 symbol=?))
                                        ; 3. update already-visited with
                                        ; the all the nodes we visited so far
                                        (append current already-visited))]))]
    ; 2. start with the first node with no nodes visited so far
    (within-n-steps?/acc s1 s2 n get-neighbors (list s1) '())))

(check-expect (within-n-steps? 'a 'a 1 (graph-neighbors g1i)) #t)
(check-expect (within-n-steps? 'a 'b 1 (graph-neighbors ga-b)) #t)
(check-expect (within-n-steps? 'a 'c 1 (graph-neighbors g3c)) #t)
(check-expect (within-n-steps? 'a 'c -1 (graph-neighbors g3c)) #f)

; 3 =========================================

; set=?/curried : [Set-of [Set-of Symbol]] -> [[Set-of [Set-of Symbol]] -> Boolean]
; Curried set=?
(define set=?/curried (λ (setofsets1)
                        (λ (setofsets2) (set-equal? setofsets1 setofsets2
                                                    (λ (set1 set2)
                                                      (set-equal? set1 set2 symbol=?))))))

(check-satisfied '((c a) (b a)) (set=?/curried '((a b) (a c))))
                                                                                      
                                                      


; find-all-paths : Graph Symbol Symbol -> [List-of [List-of Symbol]]
; finds the set of all possible acyclic paths between two given nodes in a graph
(define (find-all-paths g s1 s2)
  (find-all-paths-helper s1 s2 (graph-neighbors g)))

(check-satisfied (find-all-paths g1i 'a 'a) (set=?/curried '((a))))
(check-satisfied (find-all-paths g1s 'a 'a) (set=?/curried '((a))))
(check-satisfied (find-all-paths g2i 'a 'b) (set=?/curried '()))
(check-satisfied (find-all-paths ga->b 'a 'b) (set=?/curried '((a b))))
(check-satisfied (find-all-paths ga<-b 'a 'b) (set=?/curried '()))
(check-satisfied (find-all-paths ga-bs 'a 'b) (set=?/curried '((a b))))
(check-satisfied (find-all-paths g3i 'a 'b) (set=?/curried '()))
(check-satisfied (find-all-paths g3cs 'a 'b) (set=?/curried '((a b) (a b c))))
(check-satisfied (find-all-paths ga-b-c 'a 'c) (set=?/curried '((a b c))))
(check-satisfied (find-all-paths ga-b 'a 'b) (set=?/curried '((a b))))
(check-satisfied (find-all-paths g3c 'a 'b) (set=?/curried '((a b) (a c b))))
(check-satisfied (find-all-paths g3c 'a 'c) (set=?/curried '((a b c) (a c))))
(check-satisfied (find-all-paths gsquare 'a 'b) (set=?/curried '((a b) (a c d b))))
(check-satisfied (find-all-paths g6 'a 'b) (set=?/curried '((a c b) (a d b) (a e b) (a f b))))

(check-satisfied (find-all-paths G1 'C 'C) (set=?/curried '((C))))
(check-satisfied (find-all-paths G1 'C 'G) (set=?/curried '()))
(check-satisfied (find-all-paths G1 'A 'B) (set=?/curried '((A B))))
(check-satisfied (find-all-paths G1 'E 'G) (set=?/curried '((E F G)  (E A B F G))))
(check-satisfied (find-all-paths G1 'B 'G) (set=?/curried '((B E F G)  (B F G))))
(check-satisfied (find-all-paths G1 'A 'G) (set=?/curried '((A B E F G) (A B F G) (A E F G))))

(define (find-all-paths-helper s1 s2 get-neighbors)
  (local [; find-all-paths-helper/acc : Symbol Symbol [Symbol -> [Set-of Symbol]] [Set-of Symbol]
          ; -> [Set-of [Set-of Symbol]]
          ; produces the set of all possible acylic paths between two given nodes
          ; ACCUM: 1. represents the nodes we visited so far
          (define (find-all-paths-helper/acc s1 s2 get-neighbors already-visited)
            (cond [(symbol=? s1 s2)
                   ; 4. return the already-visited nodes to get the path
                   (list (remove-duplicates (reverse (cons s1 already-visited)) symbol=?))]
                  [(empty? (get-neighbors s1)) '()]
                  [else
                   (remove-duplicates
                    (foldr
                     (λ (s acc)
                       (append
                        ; 3. each recursive call add the current node to the nodes visited
                        (find-all-paths-helper/acc s s2 get-neighbors (cons s1 already-visited))
                        acc))
                     '()
                     (filter
                      (λ (s) (not (element-of? s already-visited symbol=?))) (get-neighbors s1)))
                    (λ (set1 set2) (set-equal? set1 set2 symbol=?)))]))]
    ; 2. start with no nodes (empty set) visited so far
    (find-all-paths-helper/acc s1 s2 get-neighbors '())))

(check-satisfied (find-all-paths-helper 'C 'C (graph-neighbors G1))
                 (set=?/curried '((C))))
(check-satisfied (find-all-paths-helper 'C 'G (graph-neighbors G1))
                 (set=?/curried '()))
(check-satisfied (find-all-paths-helper 'A 'B (graph-neighbors G1))
                 (set=?/curried '((A B))))
(check-satisfied (find-all-paths-helper 'E 'G (graph-neighbors G1))
                 (set=?/curried '((E F G)  (E A B F G))))
(check-satisfied (find-all-paths-helper 'B 'G (graph-neighbors G1))
                 (set=?/curried '((B E F G)  (B F G))))
(check-satisfied (find-all-paths-helper 'A 'G (graph-neighbors G1))
                 (set=?/curried '((A B E F G) (A B F G) (A E F G))))



; 4 ===============================================

; connected? : Graph -> Boolean
; determines if every node in a given graph can reach every other node in the graph
(define (connected? g)
  (connected-helper (graph-nodes g)
                    (graph-neighbors g) g))

; connected-helper : [Set-of Symbol] [Symbol -> [Set-of Symbol]] Graph -> Boolean
; determines if every nodes in a given Set-of Symbol can reach every other node
(define (connected-helper sos get-neighbors g)
  (andmap (λ (s1) (andmap (λ (s2) (not (empty? (find-all-paths g s1 s2)))) sos)) sos))
  
(check-satisfied g0 connected?)
(check-satisfied g1i connected?)
(check-satisfied g1s connected?)
(check-satisfied ga-bs connected?)
(check-satisfied g3cs connected?)
(check-satisfied ga->b (λ (g) (not (connected? g))))
(check-satisfied ga<-b (λ (g) (not (connected? g))))
(check-satisfied ga-b connected?)
(check-satisfied g3c connected?)
(check-satisfied gsquare connected?)


; 5 =========================================

; undirected? : Graph -> Boolean
; determines if each edge in the graph has a matching edge going the opposite direction
(define (undirected? g)
  (graph=? g (reverse-edges g)))

(check-satisfied g0 undirected?)
(check-satisfied g1i undirected?)
(check-satisfied g1s undirected?)
(check-satisfied g3c undirected?)
(check-satisfied ga-b undirected?)
(check-satisfied ga->b (λ (g) (not (undirected? g))))
(check-satisfied ga<-b (λ (g) (not (undirected? g))))
(check-satisfied gsquare undirected?)

; 6 =====================================================

(define g1i-alt (make-graph '(b) (λ (s) '())))
(define g3c-alt (make-graph '(anne ben carl)
                            (λ (s)
                              (cond [(symbol=? s 'anne) '(ben carl)]
                                    [(symbol=? s 'ben) '(anne carl)]
                                    [(symbol=? s 'carl) '(anne ben)]))))

; graph-shape=? : Graph Graph -> Boolean
; determines if two graphs have the same shapes
(define (graph-shape=? g1 g2)
  (local [(define renamed-g1 (rename g1 (build-list (length (graph-nodes g1))
                                                    (λ (n) (string->symbol (number->string n))))))
          (define renamed-g2 (rename g2 (build-list (length (graph-nodes g2))
                                                    (λ (n) (string->symbol (number->string n))))))]
    (graph=? renamed-g1 renamed-g2)))

(check-expect (graph-shape=? g0 g0) #t)
(check-expect (graph-shape=? g1i g1i-alt) #t)
(check-expect (graph-shape=? g3c g3c-alt) #t)
(check-expect (graph-shape=? ga->b ga<-b) #f)
(check-expect (graph-shape=? g3c g3cs) #f)
(check-expect (graph-shape=? ga-b-c g3cs) #f)








