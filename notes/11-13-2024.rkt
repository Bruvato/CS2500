#lang htdp/isl+

(define (max/safe m n)
  (cond [(or (not (number? m))
             (not (number? n))) (error)]
        [else (max m n)]))

; protect : {X} Prot X -> X
; ensures that the given value matches the predicate or else errors

; A Type is one of
; - 'Number
; - 'String
; - 'Boolean
; - 'Symbol
; - (make-posn Type Type)
; - (make-fun [List-of Type] Type)
(define-struct fun [args ret])

(define (pred-check pred v)
  (if (pred v) v (error)))

(define (protect t v)
  (cond [(equal? t 'Number)
         (pred-check number? v)]
        [(equal? t 'String)
         (pred-check string? v)]
        [(equal? t 'Boolean)
         (pred-check boolean? v)]
        [(equal? t 'Symbol)
         (pred-check symbol? v)]
        [(posn? t)
         (if (posn? v)
             (make-posn (protect (posn-x t) (posn-x v))
                        (protect (posn-y t) (posn-y v)))
             (error))]
        [(fun? t)
         ; (fun-args t)
         ; (fun-ret )
         (if (not (procedure? v))
             (error)
             (λ (args)
               (if (not (= (length args) (length (fun-args t))))
                   (error)
                   (protect (fun-ret t) (apply v (map protect (fun-args t) args))))))]))

