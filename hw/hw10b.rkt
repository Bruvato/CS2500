#lang htdp/isl+


; 1 =================================

; A Payemnt is one of:
; - 5
; - 10
; - 20
(define-struct cc [])
; - (make-cc)

; and represents either
; a 5 dollar bill
; a 10 dollar bill
; a 20 dollar bill
; or a credit card

(define five 5)
(define ten 10)
(define twenty 20)
(define cc0 (make-cc))

; payment-temp : Payment -> ?
(define (payment-temp p)
  (cond [(and (number? p) (= p 5)) ...]
        [(and (number? p) (= p 10)) ...]
        [(and (number? p) (= p 20)) ...]
        [(cc? p) ...]))


; tickets-sold : Natural [List-of Payment] -> Natural
; Counts how many tickets you will be able to sell
; (everyone buys one ticket for themselves)
(define (tickets-sold n lop)
  (local [; tickets-sold/acc : Natural Natural Natural [List-of Payment] -> Natural
          ; updates the accumulator with the new number of five dollar bills, ten dollar bills
          ; and number of tickets sold.
          ; 1) ACCUM: represents the current cash register
          ; with the current number of five dollar bills, ten dollar bills,
          ; and also the number of tickets sold so far
          (define (tickets-sold/acc fives tens sold lop)
            ; 4) return the number of tickets sold after we go through every payment
            ; and also uses fives and tens to determine if change can be given
            (cond [(empty? lop) sold] 
                  [(cons? lop)
                   (local [(define p (first lop))
                           (define l (rest lop))]
                     ; 3) accum updates for each payment processed in the [List-of Payment]
                     (cond [(and (number? p) (= p 5)) ; change: none
                            (tickets-sold/acc (add1 fives) tens (add1 sold) l)]
                  
                           [(and (number? p) (= p 10)) ; change: 1 five
                            (if (positive? fives)
                                (tickets-sold/acc (sub1 fives) (add1 tens) (add1 sold) l)
                                (tickets-sold/acc fives tens sold l))]
                  
                           [(and (number? p) (= p 20)) ; change: 3 fives or 1 five, 1 ten
                            (if (and (positive? tens) (positive? fives))
                                (tickets-sold/acc (sub1 fives) (sub1 tens) (add1 sold) l)
                                (if (>= fives 3)
                                    (tickets-sold/acc (- fives 3) tens (add1 sold) l)
                                    (tickets-sold/acc fives tens sold l)))]
                  
                           [(cc? p) ; change: none
                            (tickets-sold/acc fives tens (add1 sold) l)]))]))]
    ; 2) initial accumulator starts at n five dollar bills, 0 ten dollar bills,
    ; with 0 tickets sold
    (tickets-sold/acc n 0 0 lop)))


(check-expect (tickets-sold 0 '()) 0)
(check-expect (tickets-sold 0 '(5)) 1)
(check-expect (tickets-sold 0 '(5 5 5)) 3)
(check-expect (tickets-sold 1 '(10)) 1)
(check-expect (tickets-sold 1 '(10 10 10)) 1)
(check-expect (tickets-sold 2 '(5 10 20)) 3)
(check-expect (tickets-sold 0 (list (make-cc))) 1)



; 2 ======================================

; nth-smallest : Natural [NEList-of Numbers]
; obtains the nth smallest element 
; of a given a non empty list of real numbers
; given a n assumed to be less than the length
; TERMINATION: each recursive call is on a smaller subset of the
; original input list and the list can't get shorter forver
; so the list will eventually be one element and return the base case
; (or return the pivot if n falls in the "equal" range)
(define (nth-smallest n lon)
  (cond [(empty? (rest lon)) (first lon)]
        [(cons? (rest lon))
         (local [(define partitions (partition 5 lon))
                 (define medians (map median partitions))
                 (define pivot (median medians))
                 (define smaller (filter (λ (n) (< n pivot)) lon))
                 (define equal (filter (λ (n) (= n pivot)) lon))
                 (define bigger (filter (λ (n) (> n pivot)) lon))
                 (define len-smaller (length smaller))
                 (define len-equal (length equal))]
           (cond [(< n len-smaller) (nth-smallest n smaller)] ; smaller
                 [(< n (+ len-smaller len-equal)) pivot] ; equal
                 [else (nth-smallest (- n len-smaller len-equal) bigger)]))])) ; bigger
                 
(check-expect (nth-smallest 0 '(1 2 3)) 1)
(check-expect (nth-smallest 1 '(1 2 3)) 2)
(check-expect (nth-smallest 2 '(1 2 3)) 3)
(check-expect (nth-smallest 5 '(1 2 2 2 2 3)) 3)
(check-expect (nth-smallest 4 '(1 2 2 2 2 3)) 2)
(check-expect (nth-smallest 4 '(9 4 7 3 7 5 6 0)) 6)
(check-expect (nth-smallest 3 '(1 1 1 1 1 1)) 1)


; first-n : Natural [List-of Number] -> [List-of Number]
; Produces a list of the first n elements of a LON
(define (first-n n lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (if (= n 0)
             '()
             (cons (first lon) (first-n (sub1 n) (rest lon))))]))

(check-expect (first-n 1 '()) '())
(check-expect (first-n 0 '(1 2 3)) '())
(check-expect (first-n 3 '(1 2 3 4 5)) '(1 2 3))
(check-expect (first-n 5 '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (first-n 5 '(1 2 3)) '(1 2 3))

; rest-n : Natural [List-of Number] -> [List-of Number]
; Produces the rest of a LON after the first n elements
(define (rest-n n lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (if (= n 0)
             lon
             (rest-n (sub1 n) (rest lon)))]))

(check-expect (rest-n 0 '(1 2 3)) '(1 2 3))
(check-expect (rest-n 1 '()) '())
(check-expect (rest-n 4 '(1 2 3)) '())
(check-expect (rest-n 1 '(1 2 3)) '(2 3))

; partition : Natural [List-of Number] -> [List-of [List-of Number]]
; Partitions a LON into sublists of size n (n > 0)
; (with smaller lists at the end when nessecary)
(define (partition n lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (first-n n lon)
               (partition n (rest-n n lon)))]))

(check-expect (partition 1 '()) '())
(check-expect (partition 1 '(1 2 3)) '((1) (2) (3)))
(check-expect (partition 2 '(1 2 3)) '((1 2) (3)))
(check-expect (partition 5 '(1 2 3)) '((1 2 3)))


; median : [NEList-of Number] -> Number
; Calculates the exact median of a non-empty LON
(define (median lon)
  (cond [(empty? lon) (error "empty lon")]
        [(cons? lon)
         (local [(define med-l (- (/ (length lon) 2) 1))
                 (define med-r (/ (length lon) 2))
                 (define med-mid (floor (/ (length lon) 2)))
                 (define sorted-lon (sort lon <))]
           (if (odd? (length lon))
               (list-ref sorted-lon med-mid)
               (/ (+ (list-ref sorted-lon med-l)
                     (list-ref sorted-lon med-r)) 2)))]))

(check-expect (median '(1 2 3)) 2)
(check-expect (median '(1 2 3 4)) 2.5)
(check-expect (median '(2 1 4 3 5)) 3)



; 3 =========================================================

; A NumBinTree is one of
; - 'leaf
; - (make-node Num NumBinTree NumBinTree)
(define-struct node [val left right])
; and represents a Number Binary Tree

(define bt0 'leaf)
(define bt1 (make-node 1 'leaf 'leaf))
(define bt2 (make-node 1 'leaf (make-node 2 'leaf 'leaf)))
(define bt3 (make-node 1 (make-node 0 'leaf 'leaf) 'leaf))
(define bt4 (make-node 1 (make-node 0 'leaf 'leaf) (make-node 2 'leaf 'leaf)))

; bt-temp : NumBinTree -> ?
(define (bt-temp bt)
  (cond [(symbol? bt) '()]
        [(node? bt) (... (node-val bt)
                         (bt-temp (node-left bt))
                         (bt-temp (node-right bt)) ...)]))


; DO NOT USE ANY LIST FUNCTIONS EXCEPT CONS

; flatten-tree : NumBinTree -> [List-of Number]
; produces a list of all the numbers in a binary tree in left-to-right order
(define (flatten-tree bt)
  ; 4) we use the accum to build the final list in left-to-right order
  (cond [(symbol? bt) '()]
        [(node? bt)
         (local [; flatten-tree/acc : NumBinTree [List-of Number] -> [List-of Number]
                 ; produces a list of all the numbers in a BT left-to-right order
                 ; 1) ACCUM: represents the LON that have been flattened so far
                 ; from the right side of the current node
                 (define (flatten-tree/acc bt acc)
                   (local [(define val (node-val bt))
                           (define left (node-left bt))
                           (define right (node-right bt))]
                     ; 3) update the accum based on the current node's structure
                     (cond [(and (symbol? left) (symbol? right))
                            ; add current val to the accum
                            (cons val acc)]
                           [(and (symbol? (node-left bt)) (node? (node-right bt)))
                            ; adds the current val to the falttened right side
                            (cons val (flatten-tree/acc right acc))]
                           [(and (node? (node-left bt)) (symbol? (node-right bt)))
                            ; flatten the left side with the current val added to the accum
                            (flatten-tree/acc left (cons val acc))]
                           [(and (node? (node-left bt)) (node? (node-right bt)))
                            ; flatten the left side with the result of the flattened right side
                            ; (with the current val added) as the new accum
                            (flatten-tree/acc left
                                              (cons val
                                                    (flatten-tree/acc right acc)))])))]
           ; 2) the intial accum is an empty list
           (flatten-tree/acc bt '()))]))
               

(check-expect (flatten-tree bt0) '())
(check-expect (flatten-tree bt1) '(1))
(check-expect (flatten-tree bt2) '(1 2))
(check-expect (flatten-tree bt3) '(0 1))
(check-expect (flatten-tree bt4) '(0 1 2))

; 4 ========================================

; nth-value : NumBinTree Natural -> Number
; produces the n’th number counting from the left of the tree
; (where 0 indicates the leftmost element of the tree).
; TERMINATION: since we start with a non-negative valid integer index and
; decrease it by 1 (sub1 inex) each recursive call, we are guaranateed to reach
; the base case (= index 0) in a finite number of steps
(define (nth-value a-tree index)
  (cond [(symbol? a-tree) (error "invalid bt")]
        [(node? a-tree)
         (if (= index 0)
             (leftmost a-tree)
             (nth-value (remove-leftmost-node a-tree) (sub1 index)))]))

(check-error (nth-value bt0 0))
(check-expect (nth-value bt1 0) (list-ref (flatten-tree bt1) 0))
(check-error (nth-value bt1 1))
(check-expect (nth-value bt2 0) (list-ref (flatten-tree bt2) 0))
(check-expect (nth-value bt2 1) (list-ref (flatten-tree bt2) 1))
(check-expect (nth-value bt3 0) (list-ref (flatten-tree bt3) 0))
(check-expect (nth-value bt3 1) (list-ref (flatten-tree bt3) 1))
(check-expect (nth-value bt4 2) (list-ref (flatten-tree bt4) 2))

; leftmost : NumBinTree -> Number
; Obtains the value of the leftmost node of a given NumBinTree
(define (leftmost bt)
  (cond [(symbol? bt) (error "invalid bt")]
        [(node? bt)
         (local [(define val (node-val bt))
                 (define left (node-left bt))
                 (define right (node-right bt))]
           (cond [(and (symbol? left) (symbol? right)) val]
                 [(and (symbol? left) (node? right)) val]
                 [(and (node? left) (symbol? right)) (leftmost left)]
                 [(and (node? left) (node? right)) (leftmost left)]))]))

(check-error (leftmost-node bt0))
(check-expect (leftmost bt1) 1)
(check-expect (leftmost bt2) 1)
(check-expect (leftmost bt3) 0)
(check-expect (leftmost bt4) 0)

; remove-leftmost-node : NumBinTree -> NumBintTree
; Replaces the leftmost node of a NumBinTree with a leaf
(define (remove-leftmost-node bt)
  (cond [(symbol? bt) (error "invalid bt")]
        [(node? bt)
         (local [(define val (node-val bt))
                 (define left (node-left bt))
                 (define right (node-right bt))]
           (cond [(and (symbol? left) (symbol? right)) 'leaf]
                 [(and (symbol? left) (node? right)) right]
                 [(and (node? left) (symbol? right))
                  (make-node val (remove-leftmost-node left) right)]
                 [(and (node? left) (node? right))
                  (make-node val (remove-leftmost-node left) right)]))]))

(check-error (remove-leftmost-node bt0))
(check-expect (remove-leftmost-node bt1) 'leaf)
(check-expect (remove-leftmost-node bt2) (make-node 2 'leaf 'leaf))
(check-expect (remove-leftmost-node bt3) (make-node 1 'leaf 'leaf))
(check-expect (remove-leftmost-node bt4) (make-node 1 'leaf (make-node 2 'leaf 'leaf)))



; 5 ====================================

; cut-resistance : [List-of Number] -> Natural
; counts how many times we can cut a given list
; (and then cut the result, and then cut that...) until the list is empty
; TERMINATION: in each recursive call, cut-list is applied to the given LON
; which always produces a shorter list and so the list can't get shorter forever
; and will eventually reach the empty set (base case)
(define (cut-resistance lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (local [; cut-list : [List-of Number] Number -> [List-of Number]
                 ; removes one number from each run
                 (define (cut-list lon current)
                   (cond [(empty? lon) '()]
                         [(cons? lon)
                          (if (= (first lon) current)
                              (cons (first lon) (cut-list (rest lon) current))
                              (cut-list (rest lon) (first lon)))]))]
           (add1 (cut-resistance (cut-list lon (+ 1 (first lon))))))]))

(check-expect (cut-resistance '()) 0)
(check-expect (cut-resistance '(1)) 1)
(check-expect (cut-resistance '(1 1 1)) 3)
(check-expect (cut-resistance '(1 2)) 1)
(check-expect (cut-resistance '(1 1 2 2 2 3 3 3 3)) 4)
(check-expect (cut-resistance '(0 0 0 1 0 0 0)) 5)
(check-expect (cut-resistance (list 0 0 0 1 1 0 3 3 3 2 2))
              (add1 (cut-resistance (list   0 0   1     3 3   2))))
(check-expect (cut-resistance (list 0 0 1 3 3 2))
              (add1 (cut-resistance (list   0     3))))
(check-expect (cut-resistance (list 1 0 0 1 1 0 0 1 0 0 1 0 0 1 0 1 0))
              (add1 (cut-resistance (list     0   1   0     0     0))))



; 6 ===============================

; A Circle is a [CircleMessage -> Any]

(define circle-field (λ (cm) 0))
(define circle-method (λ (cm) (λ (input) input)))

; circle-temp : Circle -> ?
(define (circle-temp circle)
  (λ (cm) ...))
 
; A CircleMessage is one of:
; - 'center
; - 'radius
; - 'resize
; - 'equal
; and represents a message to circle, requesting either:
; its center (a Posn)
; its radius (a Number)
; how much to addtively change its radius by (a [Number -> Circle])
; whether or not it has the same size and position as another circle (a [Circle -> Boolean])

(define center 'center)
(define radius 'radius)
(define resize 'resize)
(define equal 'equal)

; circle-message : CircleMessage -> ?
(define (circle-message cm)
  (cond [(symbol=? cm 'center) ...]
        [(symbol=? cm 'radius) ...]
        [(symbol=? cm 'resize) ...]
        [(symbol=? cm 'equal) ...]))



; new-circle : Posn Number -> Circle
; produces a Circle given a Posn and a Number for the circle’s center and radius, respectively
(define (new-circle center radius)
  (λ (cm) (cond [(symbol=? cm 'center) center]
                [(symbol=? cm 'radius) radius]
                [(symbol=? cm 'resize) (λ (size) (new-circle center (+ radius size)))]
                [(symbol=? cm 'equal) (λ (other-circle) (and (posn-eq? center (other-circle 'center))
                                                             (= radius (other-circle 'radius))))])))
; posn-eq? : Posn Posn -> Boolean
; determines whether the x values of two given Posns are equal
; and the y values of the two POsns are equal
; (the x and y values of the two given Posns are Numbers)
(define (posn-eq? posn1 posn2)
  (and (= (posn-x posn1) (posn-x posn2))
       (= (posn-y posn1) (posn-y posn2))))


(define c0 (new-circle (make-posn 10 20) 4))
(define c1 (new-circle (make-posn 10 20) 9))
(check-expect (c0 'radius) 4)
(check-expect (c0 'center) (make-posn 10 20))
(check-expect (((c0 'resize) 10) 'radius) 14)
(check-expect ((c1 'equal) c0) #f)
(check-expect ((((c1 'resize) -5) 'equal) c0) #t)





