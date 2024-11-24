#lang htdp/isl+


; An Expr is one of:
; - String (representing a variable)

(define-struct lam [var body])
; - (make-lam String Expr)

(define-struct app [fun arg])
; - (make-app Expr Expr)

; interpretation An Expr represents an expression, and
; is either a String (which represents a variable),
; or a (make-lam v b) which represents a lambda
; expression where v is a variable bound by the
; lambda and b is the body of the lambda,
; or a (make-app f a) representing an application
; of an expression f to an argument a.

(define exp0 "x")
(define exp1 (make-lam "x" "y"))
(define exp2 (make-app "x" "y"))

; exp-temp : Expr -> ?
(define (exp-temp exp)
  (cond [(string? exp) ...]
        [(lam? exp) (... (lam-var exp) ... (exp-temp (lam-body exp)) ...)]
        [(app? exp) (... (exp-temp (app-fun exp)) ... (exp-temp (app-arg)) ...)]))


; free-bound : Expr -> Expr
; consumes an Expr and replaces all occurrences of free variables in the expression with
; "free" and others with "bound", leaving any variable v that
; appears in a (make-lam v b) unchanged.
(define (free-bound exp)
  (local [(define (free-bound/acc exp acc)   
            (cond [(string? exp) (if (element-of? exp acc) "bound" "free")]
                  [(lam? exp)
                   (make-lam (lam-var exp) (free-bound/acc (lam-body exp) (cons (lam-var exp) acc)))]
                  [(app? exp)
                   (make-app (free-bound/acc (app-fun exp) acc) (free-bound/acc (app-arg exp) acc))]))]
    (free-bound/acc exp '())))

(define (element-of? s los)
  (ormap (λ (elem) (string=? s elem)) los))





(check-expect (free-bound "x") "free")
(check-expect (free-bound (make-lam "y" "z"))
              (make-lam "y" "free"))
(check-expect
 (free-bound (make-lam "y"
                       (make-app (make-lam "z" "y")
                                 "z")))
 (make-lam "y"
           (make-app (make-lam "z" "bound")
                     "free")))



; 2 ===========================================

(define-struct rr [bases recurrence])
; An [RR X] (where RR = Recurrence Relation) is a
; (make-rr [List-of [Base X]] [Recursive X])
; interpretation A (make-rr base-cases recursive-computation)
; represents a recurrence relation where all elements of
; base-cases have unique "input"s and recursive-computation
; describes a single recursive case.

(define-struct base [input output])
; A [Base X] is a (make-base Nat X)
; interpretation A (make-base input result) tells us the result
; for a base-case input.

(define-struct recursive [gen-args combine-results])
; A [Recursive X] is a
; (make-recursive [Nat -> [List-of Nat]] [Nat [List-of X] -> X])
; interpretation A (make-recursive gen comb) describes the
; recursive case of a recurrence relation
; where gen, given a natural-number input n, generates a
; list of k inputs for k recursive calls,
; and comb, given the (same) input n, and the results
; of the k recursive calls, combines those results to
; produce the output.

(define (rr-temp rr)
  (... (lob-temp (rr-bases rr)) ... (recursive-temp (rr-recurrence rr)) ...))

(define (lob-temp lob)
  (cond [(empty? lob) ...]
        [(cons? lob) (... (base-temp (first lob))
                          (lob-temp (rest lob)) ...)]))

(define (base-temp base)
  (... (base-input base) ... (base-output base) ...))

(define (recursive-temp recursive)
  (... (recursive-gen-args recursive) ... (recursive-combine-results) ...))

; ---

(define tri
  (make-rr (list (make-base 0 0))
           (make-recursive (λ (n) (list (sub1 n)))
                           (λ (n subresults)
                             (apply + (cons n subresults))))))

(define fact
  (make-rr (list (make-base 0 1))
           (make-recursive (λ (n) (list (sub1 n)))
                           (λ (n subresults)
                             (apply * (cons n subresults))))))

(define fib
  (make-rr (list (make-base 0 1)
                 (make-base 1 1))
           (make-recursive (λ (n) (list (- n 2) (- n 1)))
                           (λ (n subresults)
                             (apply + subresults)))))

(define numbers
  (make-rr (list (make-base 0 "0"))
           (make-recursive (λ (n) (list (sub1 n)))
                           (λ (n subresults)
                             (apply string-append (append subresults (list (number->string n))))))))



; [Base X] Natural -> [Maybe X]
(define (try-get-base-case base n)
  (if (= (base-input base) n)
      (base-output base)
      #f))

; recurrence->func : [RR X] -> [Nat -> X]
(define (recurrence->func rr)
  (λ (n)
    (local [(define valid-bases
              (filter (λ (base) (not (false? (try-get-base-case base n)))) (rr-bases rr)))]
      (if (empty? valid-bases)
          (recursive-handler rr (rr-recurrence rr) n)
          (try-get-base-case (first valid-bases) n)))))

(define (recursive-handler rr recurrence n)
  (local [(define subresults
            (map (λ (gen-arg) ((recurrence->func rr) gen-arg)) ((recursive-gen-args recurrence) n)))]
    ((recursive-combine-results recurrence) n subresults)))

  
      

(check-expect (build-list 5 (recurrence->func tri))
              '(0 1 3 6 10))
(check-expect (build-list 5 (recurrence->func fact))
              '(1 1 2 6 24))
(check-expect (build-list 5 (recurrence->func fib))
              '(1 1 2 3 5))
(check-expect (build-list 5 (recurrence->func numbers))
              '("0" "01" "012" "0123" "01234"))


; 4 =====================================================================


(define-struct node (size value left right))

; A [FullTree X] is one of:
; - ’leaf
; - (make-node Nat X [FullTree X] [FullTree X])
; interpretation A [FullTree X] is either an empty tree ('leaf)
; or a node (make-node sz val l r) where sz represents the
; number of nodes (including this node itself and the nodes
; in the subtrees) that are in the tree, val is some data,
; l is the left subtree, and r is the right subtree.
; A full tree is guaranteed to be perfectly balanced, which
; means the height of its two subtrees are equal and both
; of its subtrees are perfectly balanced. In other words,
; non-empty full trees can only contain 1, 3, 7, 15, 31, etc.
; values.

(define (ft-temp ft)
  (cond [(symbol? ft) ...]
        [(node? ft) (... (nat-temp (node-size ft))
                         (node-value ft)
                         (ft-temp (node-left ft))
                         (ft-temp (node-right ft)) ...)]))
                         

(define LEAF 'leaf)
(define ft1 (make-node 1 'good LEAF LEAF))
(define ft2 (make-node 3 'hello
                       ft1
                       (make-node 1 'bye LEAF LEAF)))
(define ft3 (make-node 3 'so
                       (make-node 1 'it LEAF LEAF)
                       (make-node 1 'is LEAF LEAF)))
(define ft4 (make-node 7 'foo ft2 ft3))

; tree-combine : X [FullTree X] [FullTree X] -> [FullTree X]
; generates a new full tree
(define (tree-combine root left-ft right-ft)
  (cond [(symbol? left-ft) (make-node 1 root 'leaf 'leaf)]
        [(node? left-ft) (make-node (new-size (node-size left-ft))
                                    root
                                    left-ft
                                    right-ft)]))

(define (new-size size)
  (+ size (add1 size)))

(check-expect (tree-combine "a" LEAF LEAF) (make-node 1 "a" LEAF LEAF))
(check-expect (tree-combine "a" ft1 ft1) (make-node 3 "a" ft1 ft1))
(check-expect (tree-combine "a" ft2 ft2) (make-node 7 "a" ft2 ft2))


; 5 ================================
  
; tree-ref : [FullTree X] Natural : X
(define (tree-ref ft n)
  (cond [(and (symbol? ft) (zero? n)) (error)]
        [(and (symbol? ft) (positive? n)) (error)]
        [(and (node? ft) (zero? n)) (node-value ft)]
        [(and (node? ft) (positive? n))
         (if (< (sub1 n) (node-size (node-left ft)))
             (tree-ref (node-left ft) (sub1 n))
             (tree-ref (node-right ft) (- (node-size (node-right ft)) (- (node-size ft) n))))]))

(check-expect (tree-ref ft1 0) 'good)
(check-expect (tree-ref ft2 0) 'hello)
(check-expect (tree-ref ft2 1) 'good)
(check-expect (tree-ref ft2 2) 'bye)
(check-expect (build-list 7 (λ (i) (tree-ref ft4 i)))
              '(foo hello good bye so it is))


; 6 ============================================

; A [QuickList X] is a [List-of [FullTree X]]
; and represents a list of X values, where if the
; first tree of the quick list is a full tree of
; size n, then the first n elements of the
; represented list are contained in the first tree.
; All of the trees in a quick list are guaranteed
; to be non-empty.

; quicklist-ref : [QuickList X] Natural -> X
;  produces the n-th element of the represented list
(define (quicklist-ref ql n)
  (local [(define (quicklist-ref/acc ql n fl-visited)
            (cond [(empty? ql) (error)]
                  [(cons? ql) (if (>= (sub1 (node-size (first ql))) (- n fl-visited))
                                  (tree-ref (first ql) (- n fl-visited))
                                  (quicklist-ref/acc (rest ql) n (add1 fl-visited)))]))]
    (quicklist-ref/acc ql n 0)))

(check-expect
 (build-list 4
             (λ (i) (quicklist-ref (list ft1 ft3) i)))
 '(good so it is))

; 7 =====================================================

; A [QuickList X] is a [List-of [FullTree X]]
; and represents a list of X values, where if the
; first tree of the quick list is a full tree of
; size n, then the first n elements of the
; represented list are contained in the first tree.
; All of the trees in a quick list are guaranteed
; to be non-empty.
; All of the trees are guaranteed to be successively
; larger, with the possible exception of the first
; two trees being the same size.

; quick-cons : X [QuickList X] -> [QuickList X]
; adds a given value to the quick list



(define (quick-cons value ql)
  (if (and (cons? ql) (cons? (rest ql)) (same-size-fr (first ql) (second ql)))
      (cons (tree-combine value (first ql) (second ql)) (rest (rest ql)))
      (cons (make-node 1 value 'leaf 'leaf) ql)))

; [FullTree X] [FullTree X] -> Boolean
(define (same-size-fr fr1 fr2)
  (= (node-size fr1) (node-size fr2)))

(check-expect (quick-cons 'good '()) (list ft1))
(check-expect (quick-cons 'good (list ft1)) (list ft1 ft1))
(check-expect (quick-cons 'a (list ft1 ft1)) (list (tree-combine 'a ft1 ft1)))
(check-expect (quick-cons 'a (list ft1 ft1 ft1)) (list (tree-combine 'a ft1 ft1) ft1))
(check-expect (quick-cons 'good (list ft1 ft2)) (list ft1 ft1 ft2))
(check-expect (quick-cons 'good (list ft1 ft2 ft3)) (list ft1 ft1 ft2 ft3))

                        










