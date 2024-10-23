#lang htdp/isl+

; An Ancestor is either 'mother or 'father
; an AP [Ancestor Path] is a [List-of Ancestor]

(define-struct person [name mother father])
; A FT (Family Tree) is one of
; - 'unknown
; - (make-person String FT FT)

; given an ancestor path and a family tree, determine the name of
; the designated ancestor: if there isnt one, return false

; cross product temp

(define (name-of ap ft)
  (cond
    [(and (symbol? ft) (empty? ap)) #f]
    [(and (symbol? ft) (cons? ap)) #f]
    [(and (person? ft) (empty? ap)) (person-name ft)]
    [(and (person? ft) (cons? ap))
     (if (symbol=? (first ap) 'mother)
         (name-of (rest ap) (person-mother ft))
         (name-of (rest ap) (person-father ft)))]))


; A DescendantTree is (make-person String Kids)
; A Kids is a [List-of DescendantTree]
(define-struct person [name kids])

(define Ben (make-person "Ben" '()))

; mutually recursive data def
(define (dt-temp dt)
  (... (person-name dt)
       (kids-temp (person-kid dt))))
(define (kids-temp kids)
  (cond [(empty? kids) ...]
        [(cons? kids)
         (... (dt-temp (first kids))
              (kids-temp (rest kids)))]))

; does this dt have a desc named name?
(define (has-descendant-named? dt name)
  (or (string=? (person-name dt) name)
      (kids-helper (person-kids dt) name)))
(define (kids-helper kids name)
  ;(ormap (λ(kid) (has-descendant-named? kid name)) kids)
  (cond [(empty? kids) #f]
        [(cons? kids)
         (or (has-descendant-named? (first kids) name)
             (kids-helper (rest kids) name))]))
              