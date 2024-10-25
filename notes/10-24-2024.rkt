#lang htdp/isl+

; ========== trees warm up =============

; given an S-expr
; - triple each #
; - not each bool
; - stirngs -> length
; - symbols stay put

; transform : SExpr -> SExpr
(define (transform sexpr)
  (cond [(atom? sexpr) (transform-atom sexpr)]
        [(list? sexpr) (transform-list sexpr)]))

; 
(define (transform-atom a)
  (cond [(number? a) (* 3 a)]
        [(boolean? a) (not a)]
        [(string? a) (string-length a)]
        [(symbol? a) a]))

(define (transofmr-list losexpr)
  (map transorm losexpr))

; ==============================

; An ArithExpr (AE) is one of:
; - Number
; - (cons FuncName [List-of AE])
; A FuncName is one of
; - '+
; - '-
; - '*
; - '/
; - 'sqrt

; is-arith-expr? : SExpr -> Boolean
(define (is-arith-expr sexpr)
  (cond [(atom? sexpr) (number? sexpr)]
        [(list? sexpr) is-arith-functioncall?]))

(define (is-arith-functioncall? losexpr)
  (cond [(empty? losexpr) #f]
        [(cons? losexpr)
         (and (is-func-name? (first losexpr))
              (andmap is-arith-expr (rest losexpr)))]))

; SExpr -> 
(define (is-func-name? sexpr)
  (and 
  (symbol? sexpr)
  (member? sexpr
           '(+ - * / sqrt))))

; evaluate : AE -> Number
(define (evaluate ae)
  (cond [(number? ae) ae]
        [(symbol? ae)
         (local [(define fn (first ae))
                 (define args (rest ae))
                 (define args-values (map evaluate args))]
           (apply-fn fn args-values))]))
(define (apply-fn fn vals)
  (cond [(symbol=? fn '+) (foldr + 0 vals)]
        [(symbol=? fn '-) (if (empty? vals) (error '- "no args") (foldl - (first vals) (rest vals)))]
        [(symbol=? fn '*) (foldr * 1 vals)]
        [(symbol=? fn '/) (if (and (= 2 (length vals))
                              (not (= (second vals) 0)))
                              ...)]
        [(symbol=? fn 'sqrt) ...]))
        
        
         
         