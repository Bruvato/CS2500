;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)


(define (score-by-length words)
  (if (empty? words)
  0
  (+ (string-length (first words))
     (score-by-length (rest words)))))


(define (overlay-all images)
  (if (empty? images)
      (rectangle 10 10 "solid" "white")
      (overlay (first images)
               (overlay-all (rest images)))))


(define (bar-graph heights)
  (if (empty? heights)
      (rectangle 1 1 "solid" "white")
      (beside/align "bottom"
                    (rectangle 10 (first heights) "solid" "black")
                    (bar-graph (rest heights)))))


(define (is-in? item list)
  (cond
    [(empty? list) #false]
    [(equal? item (first list)) #true]
    [else (is-in? item (rest list))]))


(define (words-to-sentence words)
  (cond
    [(empty? words) ""]
    [(empty? (rest words)) (first words)]
    [else (string-append (first words) " " (words-to-sentence (rest words)))]))

