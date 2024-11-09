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

(define S1 '(anne))
(define S2 '(anne billie))
(define S3 '(anne billie carl))
(define S4 '(anne billie carl dave)) 

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
       (graph-neighbors g) ...))  ; ?????

; 1 --------------------------------------

(define g0 (make-graph '() (λ (s) '()))) ; no nodes
(define g1 (make-graph '(a) (λ (s) '()))) ;1 node
(define g2 (make-graph '(a b)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(b)]
                               [(symbol=? s 'b) '(a)])))) ; 2 connected nodes
(define g3 (make-graph '(a b) (λ (s) '()))) ; 2 isolated nodes
(define g4 (make-graph '(a b c)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(b c)]
                               [(symbol=? s 'b) '(a c)]
                               [(symbol=? s 'c) '(a b)])))) ; 3 connected nodes
(define g5 (make-graph '(a b c)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(b)]
                               [(symbol=? s 'b) '(a c)]
                               [(symbol=? s 'c) '(b)])))) ; a - b - c
(define g6 (make-graph '(a b c d)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(b c)]
                               [(symbol=? s 'b) '(a)]
                               [(symbol=? s 'c) '(a d)]
                               [(symbol=? s 'd) '(c)]))))
(define g7 (make-graph '(a b c d)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(b)]
                               [(symbol=? s 'b) '()]
                               [(symbol=? s 'c) '(d)]
                               [(symbol=? s 'd) '()]))))
(define g8 (make-graph '(a b)
                       (λ (s)
                         (cond [(symbol=? s 'a) '(b)]
                               [(symbol=? s 'b) '()]))))

(define G1 (make-graph
            S1
            (λ (name)
              (cond [(symbol=? name 'anne) '()]))))
(define G2 (make-graph
            S2
            (λ (name)
              (cond [(symbol=? name 'anne) '(anne billie)]
                    [(symbol=? name 'billie) '(billie anne)]))))
(define G3 (make-graph
            S3
            (λ (name)
              (cond [(symbol=? name 'anne) '(billie carl)]
                    [(symbol=? name 'billie) '(anne carl)]
                    [(symbol=? name 'carl) '(billie anne)]))))
(define G4 (make-graph
            S4
            (λ (name) 
              (cond [(symbol=? name 'anne)   '(billie carl)]
                    [(symbol=? name 'billie) '(anne)]
                    [(symbol=? name 'carl)   '(anne dave)]
                    [(symbol=? name 'dave)   '(carl)]))))
(define G5 (make-graph
            S4
            (λ (name) 
              (cond [(symbol=? name 'anne)   '(billie)]
                    [(symbol=? name 'billie) '()]
                    [(symbol=? name 'carl)   '(dave)]
                    [(symbol=? name 'dave)   '()]))))

; 2 -----------------------------------------

; neighbor-of? : Graph Symbol Symbol -> Boolean
; determines if the second symbol is a neighbor of the first one in a Graph
; (Assume both symbols are in the graph)
(define (neighbor-of? g s1 s2)
  (element-of? s2 ((graph-neighbors g) s1) symbol=?))

(check-expect (neighbor-of? (make-graph '(a) (λ (name) '(a))) 'a 'a) #t)
(check-expect (neighbor-of? g1 'a 'a) #f)
(check-expect (neighbor-of? g2 'a 'b) #t)
(check-expect (neighbor-of? g2 'b 'a) #t)
(check-expect (neighbor-of? g3 'a 'b) #f)
(check-expect (neighbor-of? g4 'a 'b) #t)
(check-expect (neighbor-of? g4 'a 'c) #t)
(check-expect (neighbor-of? g5 'a 'c) #f)
(check-expect (neighbor-of? (make-graph '(a b c d) (λ (s) (cond [(symbol=? s 'a) '(b c)]
                                                                [(symbol=? s 'b) '(a)]
                                                                [(symbol=? s 'c) '(a d)]))) 'a 'd) #f)

; element-of? : {X} X [Set-of X] [X X -> Boolean]
; determines if X is an element of a Set-of X
(define (element-of? x s e?)
  (ormap (λ (elem) (e? x elem)) s))

(check-expect (element-of? 1 '() =) #f)
(check-expect (element-of? 1 '(1 2 3) =) #t)
(check-expect (element-of? "hi" (list "a" "hello" "hi") string=?) #t)
(check-expect (element-of? 'a '(d b a c b) symbol=?) #t)

; 3 ------------------------------------
; union : [Set-of X] [Set-of X] [X X -> Boolean] -> [Set-of X]
; takes the union of two sets
(define (union set1 set2 e?)
  (remove-duplicates (append set1 set2) e?))

(check-expect (set-equal? (union '(1 2) '() =) '(1 2) =) #t)
(check-expect (set-equal? (union '(1 2) '(3 4) =) '(1 2 3 4) =) #t)
(check-expect (set-equal? (union '(2 1) '(4 3) =) '(1 2 3 4) =) #t)
(check-expect (set-equal? (union '(1 2) '(2 3) =) '(1 2 3) =) #t)
(check-expect (set-equal? (union '(a b) '(a b) symbol=?) '(a b) symbol=?) #t)
(check-expect (set-equal? (union '(a) '(a) symbol=?) '() symbol=?) #f)

; remove-duplicates : [List-of X] [X X -> Boolean] -> [List-of X]
(define (remove-duplicates l e?)
  (foldr (λ (elem acc) (if (element-of? elem acc e?) acc (cons elem acc))) '() l))

(check-expect (remove-duplicates '(1 1 2) =) '(1 2))
(check-expect (remove-duplicates '(1 2) =) '(1 2))
(check-expect (remove-duplicates '(1 2 2 2 3 3 4 4 4 4) =) '(1 2 3 4))

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

(check-expect (set-equal? '() '() =) #t)
(check-expect (set-equal? '(1 2) '(1 2) =) #t)
(check-expect (set-equal? '(1 2) '(2 1) =) #t)
(check-expect (set-equal? '(1) '(1 2) =) #f)
(check-expect (set-equal? '(a b c d) '(b a d c) symbol=?) #t)

; subset? : [Set-of X] [Set-of X] [X X -> Boolean] -> Boolean
; determines if the first set is a subset of the second set
(define (subset? s1 s2 e?)
  (andmap (λ (elem1) (element-of? elem1 s2 e?)) s1))

(check-expect (subset? '() '(1) =) #t)
(check-expect (subset? '(1) '(1) =) #t)
(check-expect (subset? '(1) '(1 2) =) #t)
(check-expect (subset? '(1 2) '(2 1) =) #t)
(check-expect (subset? '(a b) '(c d b a) symbol=?) #t)




; both-neighbors : Symbol Symbol Graph -> [Set-of Symbol]
; takes two symbols and a graph and returns the list of both of their neighbors
; (no duplicates in outpute,  Assume both symbols are in the graph)
(define (both-neighbors s1 s2 g)
  (union-both-neighbors s1 s2 (graph-neighbors g)))

(define (union-both-neighbors s1 s2 get-neighbors)
  (union (get-neighbors s1)
         (get-neighbors s2) symbol=?))

(check-expect (set-equal? (union-both-neighbors 'a 'a (λ (name) '(a))) '(a) symbol=?) #t)
(check-expect (set-equal? (union-both-neighbors 'a 'b (λ (name) '())) '() symbol=?) #t)
(check-expect (set-equal? (union-both-neighbors 'a 'b
                                                (λ (s)
                                                  (cond [(symbol=? s 'a) '(b)]
                                                        [(symbol=? s 'b) '(a)])))
                          '(a b) symbol=?) #t)

(check-expect (set-equal? (both-neighbors 'a 'a (make-graph '(a)
                                                            (λ (name) '(a)))) '(a) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'a 'a g1) '() symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'a 'a g2) '(b) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'a 'b g2) '(a b) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'b 'a g2) '(a b) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'a 'b g3) '() symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'a 'b g4) '(a b c) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'd 'b g6) '(a c) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'a 'c g6) '(a b c d) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'a 'c g7) '(b d) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'dave 'billie G4) '(anne carl) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'anne 'billie G2) '(anne billie) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'anne 'billie G3) '(anne billie carl) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'anne 'carl G4) '(anne billie carl dave) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'anne 'carl G5) '(billie dave) symbol=?) #t)
(check-expect (set-equal? (both-neighbors 'a 'a g1) '(a) symbol=?) #f)
(check-expect (set-equal? (both-neighbors 'a 'b g2) '() symbol=?) #f)
(check-expect (set-equal? (both-neighbors 'a 'c g7) '(a b d) symbol=?) #f)
(check-expect (set-equal? (both-neighbors 'a 'b g8) '(b) symbol=?) #t)


; 4 -----------------------------------------

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

(check-expect (graph=? G2 (make-graph
                           '(billie anne)
                           (λ (name)
                             (cond [(symbol=? name 'anne) '(anne billie)]
                                   [(symbol=? name 'billie) '(billie anne)]))))
              #t)
(check-expect (graph=? G3 (make-graph
                           '(carl anne billie)
                           (λ (name)
                             (cond [(symbol=? name 'anne) '(billie carl)]
                                   [(symbol=? name 'billie) '(anne carl)]
                                   [(symbol=? name 'carl) '(billie anne)]))))
              #t)
(check-expect (graph=? G2 (make-graph
                           '(billie anne)
                           (λ (name)
                             (cond [(symbol=? name 'anne) '(anne billie)]
                                   [(symbol=? name 'billie) '(billie anne)]))))
              #t)
(check-expect (graph=? G4 (make-graph
                           '(dave anne billie carl)
                           (λ (name) 
                             (cond [(symbol=? name 'anne)   '(billie carl)]
                                   [(symbol=? name 'billie) '(anne)]
                                   [(symbol=? name 'carl)   '(anne dave)]
                                   [(symbol=? name 'dave)   '(carl)]))))
              #t)
(check-expect (graph=? G4 (make-graph
                           '(dave anne billie carl)
                           (λ (name) 
                             (cond [(symbol=? name 'anne)   '(carl billie)]
                                   [(symbol=? name 'billie) '(anne)]
                                   [(symbol=? name 'carl)   '(dave anne)]
                                   [(symbol=? name 'dave)   '(carl)]))))
              #t)
(check-expect (graph=? G5 (make-graph
                           S4
                           (λ (name) 
                             (cond [(symbol=? name 'anne)   '(billie)]
                                   [(symbol=? name 'billie) '()]
                                   [(symbol=? name 'carl)   '(dave)]
                                   [(symbol=? name 'dave)   '()]))))
              #t)
(check-expect (graph=? G5 (make-graph
                           S4
                           (λ (name) 
                             (cond [(symbol=? name 'anne)   '(billie)]
                                   [(symbol=? name 'billie) '()]
                                   [(symbol=? name 'carl)   '(dave)]
                                   [(symbol=? name 'dave)   '()]))))
              #t)
(check-expect (graph=? G5 (make-graph
                           S4
                           (λ (name) 
                             (cond [(symbol=? name 'anne)   '()]
                                   [(symbol=? name 'billie) '()]
                                   [(symbol=? name 'carl)   '(dave)]
                                   [(symbol=? name 'dave)   '()]))))
              #f)
(check-expect (graph=? G5 (make-graph
                           S3
                           (λ (name) 
                             (cond [(symbol=? name 'anne)   '(billie)]
                                   [(symbol=? name 'billie) '()]
                                   [(symbol=? name 'carl)   '(dave)]
                                   [(symbol=? name 'dave)   '()]))))
              #f)

; 5 ------------------------------------

; graph=?/curried : Graph -> [Graph -> Boolean]
; Curried graph=?
(define graph=?/curried (λ (g1) (λ (g2) (graph=? g1 g2))))

; graph=?/some-expected-graph : Graph -> Boolean
#;(define graph=?/some-expected-graph (graph=?/curried some-expected-graph))
; This function checks whether any given graph is equal to some-expected-graph

; f : Graph -> Graph
; Do something to graph g
#;(define (f g ...) ...)
 
#;(check-satisfied (f some-input-graph ...)
                   (graph=?/curried some-expected-graph))

; collapse : Symbol Symbol Symbol Graph -> Graph
; collapses the first two nodes into one new node, which is named by the third node
(define (collapse s1 s2 s3 g)
  (make-graph (collapse-nodes s1 s2 s3 (graph-nodes g))
              (new-get-neighbors s1 s2 s3
                                 (graph-neighbors g)
                                 g)))

; collapse-nodes : Symbol Symbol Symbol [List-of Symbols
(define (collapse-nodes s1 s2 s3 nodes)
  (remove s1 (remove s2 (cons s3 nodes))))

(check-expect (set-equal? (collapse-nodes 'a 'b 'c '(a b)) '(c) symbol=?) #t)
(check-expect (set-equal? (collapse-nodes 'a 'b 'd '(a b c)) '(c d) symbol=?) #t)

; new-get-neighbors : 
(define (new-get-neighbors s1 s2 s3 get-neighbors g)
  (λ (name) (if (symbol=? name s3)
                (new-neighbors s1 s2 s3 (both-neighbors s1 s2 g))
                (new-neighbors s1 s2 s3 (get-neighbors name)))))

(define (new-neighbors s1 s2 s3 neighbors)
  (remove-duplicates (map (λ (neighbor) (cond [(symbol=? neighbor s1) s3]
                                              [(symbol=? neighbor s2) s3]
                                              [else neighbor])) neighbors) symbol=?))

(check-satisfied (collapse 'a 'a 'b g1)
                 (graph=?/curried (make-graph '(b) (λ (s) '())))) ; 1 isolated
(check-satisfied (collapse 'a 'b 'c g2)
                 (graph=?/curried (make-graph '(c) (λ (s) '(c))))) ; 2 connected
(check-satisfied (collapse 'a 'b 'c g3)
                 (graph=?/curried (make-graph '(c) (λ (s) '())))) ; 2 isolated
(check-satisfied (collapse 'a 'b 'd g4)
                 (graph=?/curried (make-graph '(d c)
                                              (λ (s) (cond [(symbol=? s 'd) '(c d)]
                                                           [(symbol=? s 'c) '(d)]))))) ; 3 connected
(check-satisfied (collapse 'a 'b 'd g5)
                 (graph=?/curried (make-graph '(c d)
                                              (λ (s) (cond [(symbol=? s 'd) '(c d)]
                                                           [(symbol=? s 'c) '(d)]))))) ; a - b - c
(check-satisfied (collapse 'anne 'anne 'mango
                           (make-graph '(anne)
                                       (λ (name)
                                         (cond [(symbol=? name 'anne) '(anne)]))))
                 (graph=?/curried (make-graph '(mango)
                                              (λ (name) '(mango)))))
(check-satisfied (collapse 'anne 'billie 'mango G2)
                 (graph=?/curried (make-graph '(mango)
                                              (λ (name) '(mango)))))
(check-satisfied (collapse 'anne 'billie 'mango G3)
                 (graph=?/curried (make-graph '(mango carl)
                                              (λ (name)
                                                (cond [(symbol=? name 'mango) '(mango carl)]
                                                      [(symbol=? name 'carl) '(mango)])))))

; 6 -----------------------------------------
  
; reverse-edges : Graph -> Graph
; 
(define (reverse-edges g)
  (make-graph (graph-nodes g) (λ (name)
                                (filter (λ (node) (neighbor-of? g node name)) (graph-nodes g)))))

(check-satisfied (reverse-edges g1)
                 (graph=?/curried (make-graph '(a) (λ (s) '()))))
(check-satisfied (reverse-edges g2)
                 (graph=?/curried (make-graph '(b a) (λ (s) (cond [(symbol=? s 'a) '(b)]
                                                                  [(symbol=? s 'b) '(a)])))))
(check-satisfied (reverse-edges g3)
                 (graph=?/curried (make-graph '(b a) (λ (s) '()))))
(check-satisfied (reverse-edges g7)
                 (graph=?/curried (make-graph '(a b c d)
                                              (λ (s)
                                                (cond [(symbol=? s 'a) '()]
                                                      [(symbol=? s 'b) '(a)]
                                                      [(symbol=? s 'c) '()]
                                                      [(symbol=? s 'd) '(c)])))))


; rename : Graph [List-of Symbol] -> Graph
(define (rename g los)
  (make-graph
   los
   (new-func (graph-nodes g) (graph-neighbors g) g los)))

(define (new-func nodes get-neighbors g los)
  (λ (name) (map (λ (node) (get-nth node nodes los)) (get-neighbors (get-nth name los nodes)))))
              


(define (get-nth s los1 los2)
  (cond [(empty? los1) (error "elem not in los")]
        [(cons? los1) (if (symbol=? s (first los1))
                          (first los2)
                          (get-nth s (rest los1) (rest los2)))]))

(check-expect (get-nth 'a '(a b) '(x y)) 'x)
(check-expect (get-nth 'b '(a b) '(x y)) 'y)
(check-expect (get-nth 'c '(a b c) '(x y z)) 'z)

(check-satisfied (rename g1 '(x))
                 (graph=?/curried (make-graph '(x) (λ (s) '()))))
(check-satisfied (rename g2 '(x y))
                 (graph=?/curried (make-graph '(x y) (λ (s) (cond [(symbol=? s 'x) '(y)]
                                                                  [(symbol=? s 'y) '(x)])))))
(check-satisfied (rename g4 '(x y z))
                 (graph=?/curried (make-graph '(x y z) (λ (s) (cond [(symbol=? s 'x) '(y z)]
                                                                    [(symbol=? s 'y) '(x z)]
                                                                    [(symbol=? s 'z) '(x y)])))))



