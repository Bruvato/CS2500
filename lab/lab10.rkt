#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)

; A KMCA (K-Means Clustering Assignment) is a
; (make-assignment [List-of Centroid] [List-of Nat] Boolean)
(define-struct assignment [centroids labels no-reassignment?])
; - where centroids is the current list of centroids (ordered from 0...k-1)
; - labels are the labels assigned to each datapoint
;   labels are indices into the centroids list
;   (the first datapoint is labeled with the first element in labels,
;    second point the second label, etc.)
; - and no-reassignment? keeps track of whether or not re-assignment has occurred
 
; A Centroid is a (make-centroid Number Number)
(define-struct centroid [x y])
; - where x is the x-coordinate of the centroid
; - and y is the y-coordinate of the centroid

; Examples of Centroid:
(define CENTROID-0 (make-centroid 0 0))
(define CENTROID-1 (make-centroid -3 -4))
(define CENTROID-2 (make-centroid 10 10))
 
; A Datapoint is a (make-datapoint Number Number)
(define-struct datapoint [x y])
; - where x is the x-coordinate of the data point
; - and y is the y-coordinate of the data point

; Examples of Datapoint
(define DATAPOINT-0 (make-datapoint 0 0))
(define DATAPOINT-1 (make-datapoint -3 -4))
(define DATAPOINT-2 (make-datapoint 10 10))

; main : PosInt [List-of Datapoint] -> [List-of [List-of Datapoint]]
; run k-means clustering and output the datapoints binned into their respective clusters
#;(define (main k lop)
    (local
      [; next-assignment/main : KMCA -> KMCA
       ; Advance to the next assignment
       (define (next-assignment/main assignment)
         (next-assignment assignment k lop))
       ; draw/main : KMCA -> KMCA
       ; Draw the current clusters and points
       (define (draw/main assignment)
         (draw assignment k lop))]
      (cluster
       (assignment-labels
        (big-bang (initial-assignment k lop)
          [on-tick next-assignment/main 1]
          [to-draw draw/main]
          [stop-when assignment-no-reassignment?]))
       k lop)))

; A Nat is one of
; - 0
; - (add1 Nat)

; EXERCISE 1 --------------------

; list-segment: {X} Nat [List-of X] -> [List-of X]
; Given a Nat n and a list, outputs the first n elements of the given list.
; Returns the full list if n > the length of the list.
(define (list-segment n lox)
  (cond [(or (<= n 0) (empty? lox)) '()]
        [(>= n (length lox)) lox]
        [(cons? lox) (cons (first lox) (list-segment (sub1 n) (rest lox)))]))

; tests for list-segment
(define TEST-LIST '(1 2 3 4 5))
(check-expect (list-segment 0 TEST-LIST) '())
(check-expect (list-segment 1 TEST-LIST) '(1))
(check-expect (list-segment 3 TEST-LIST) '(1 2 3))
(check-expect (list-segment 5 TEST-LIST) TEST-LIST)
(check-expect (list-segment 10 TEST-LIST) TEST-LIST)

; EXERCISE 2 --------------------

; distance: Datapoint Centroid -> Number
; Calculates the euclidean distance between the given Datapoint and Centroid
(define (distance dp ct)
  (sqrt (+ (sqr (- (datapoint-x dp) (centroid-x ct)))
           (sqr (- (datapoint-y dp) (centroid-y ct))))))

; tests for distance
(check-within (distance DATAPOINT-0 CENTROID-1) 5 0.0001)
(check-within (distance DATAPOINT-1 CENTROID-0) 5 0.0001)

; EXERCISE 3 --------------------

; average: [List-of Number] -> Number
; Averages the numbers in the given list
(define (average lon)
  (/ (foldr + 0 lon)
     (length lon)))

; tests for average
(check-expect (average TEST-LIST) 3)

; mean-centroid: [List-of Datapoint] -> Number
; Given a list of Datapoints, create a Centroid whose coordinates are the means
; of the coordinates of the Datapoints
(define (mean-centroid lod)
  (make-centroid
   (average (map datapoint-x lod))
   (average (map datapoint-y lod))))

; tests for mean-centroid
(check-expect (mean-centroid (list DATAPOINT-0 DATAPOINT-1 DATAPOINT-2))
              (make-centroid (/ 7 3) (/ 6 3)))

; EXERCISE 4 --------------------

; mean-centroids: [List-of [List-of Datapoint]] -> [List-of Centroid]
; Produces the mean centroid for each list of Datapoints
(define (mean-centroids lolod)
  (map mean-centroid lolod))

; tests for mean-centroids
(check-expect
 (mean-centroids
  (list
   (list DATAPOINT-0 DATAPOINT-2)
   (list DATAPOINT-0 DATAPOINT-1 DATAPOINT-2)))
 (list
  (make-centroid 5 5)
  (make-centroid (/ 7 3) (/ 6 3))))

; EXERCISE 5 --------------------

; cluster : {X} [List-of Nat] Nat [List-of X] -> [List-of [List-of X]]
; Given a list of labels and the number of clusters, cluster lox
; (length labels) = (length lox)

(define (cluster labels k lox)
  (build-list k (λ (cluster)
                  (foldr (λ (label x acc) (if (= label cluster)
                                              (cons x acc)
                                              acc)) '() labels lox))))

(check-expect (cluster '(0)
                       1
                       (list "a"))
              (list (list "a")))
(check-expect (cluster '(0 0)
                       1
                       (list "a" "b"))
              (list (list "a" "b")))
(check-expect (cluster '(0 1)
                       2
                       (list "a" "b"))
              (list (list "a")
                    (list "b")))
(check-expect (cluster (list 0 1 3 0 1 3)
                       4
                       (list "a" "b" "c" "d" "e" "f"))
              (list (list "a" "d")
                    (list "b" "e")
                    '()
                    (list "c" "f")))


; EXERCISE 6 --------------------

; assign-new-labels : [List-of Centroid] Nat [List-of Datapoint]  -> [List-of Nat]
; Given the current centroids and the list of data points, output the new labels
(check-expect (assign-new-labels (list (make-centroid -5 -5) (make-centroid 5 5))
                                 2
                                 (list (make-datapoint -20 -20)
                                       (make-datapoint -3 1)
                                       (make-datapoint 20 20)))
              (list 0 0 1))
(define (assign-new-labels centroids k lodp)
  (map (λ (dp)
         (second (argmin
                  (λ (pair) (distance dp (first pair)))
                  (build-list
                   (length centroids)
                   (λ (i) (list (list-ref centroids i) i)))))) lodp))



  
       
       





