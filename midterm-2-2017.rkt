#lang htdp/isl+



; 1.

; a LOS is one of:
; - '()
; - (cons String LOS)

(define los0 '())
(define los1 (list "hi"))
(define los2 (list "hi" "hello"))
(define los3 (list "hi" "hello" "hey"))

#;(define (los-temp los)
    (cond [(empty? los) ...]
          [(cons? los) (... (first los) ... (los-temp (rest los)) ...)]))

; echo : [List-of String] -> String
; creates a string that contains every stirng in a given LOS
; followed by a String that represents the length of the
; original one
(define (echo los)
  (local [; echo/acc : [List-of String] String -> String
          ; creates a string that contains every string in a given LOS
          ; followed by a String that reps the len of the original one
          ; ACCUM: accumulates each string in the los with its length onto a result
          ; 1. represents the accumulated string so far
          (define (echo/acc los result)
            ; 4. return the accumulated result when we've gone through all the strings in the los
            (cond [(empty? los) result]
                  [(cons? los)
                   ; 3. update the accumulated result until the los is empty
                   (echo/acc (rest los)
                             (string-append result
                                            (first los)
                                            (number->string (string-length (first los)))))]))]
    ; 2. start with the given los and the empty string
    (echo/acc los "")))


(check-expect (echo los0) "")
(check-expect (echo los1) "hi2")
(check-expect (echo los2) "hi2hello5")
(check-expect (echo los3) "hi2hello5hey3")


; 2.

; A BaseType is one of:
; -- 'Number
; -- 'String
; -- 'Symbol
; -- 'Boolean
; -- 'Image

(define basetype0 3)
(define basetype1 "hi")
(define basetype2 'a)
(define basetype3 #t)

(define (basetype-temp basetype)
  (cond [(symbol=? basetype 'Number) ...]
        [(symbol=? basetype 'String) ...]
        [(symbol=? basetype 'Symbol) ...]
        [(symbol=? basetype 'Boolean) ...]
        [(symbol=? basetype 'Image) ...]))

(define-struct => [domain range])
(define-struct union [parts])
; A Signature is one of:
; -- Symbol
; -- (make-=> List-of-Signatures Signature)
; -- (make-union List-of-Signatures)

(define s0 'Number)
(define s1 (make-=> '(Number String Boolean) 'Number))
(define s2 (make-union '(Number String Boolean)))

(define (sig-temp sig)
  (cond [(symbol? sig) ...]
        [(=>? sig) (... (losig-temp (=>-domain sig)) ... (sig-temp (=>-range sig)) ...)]
        [(union? sig) (... (losig-temp (union-parts sig)) ...)]))
;
; A List-of-Signatures is one of:
; -- (cons Signature '())
; -- (cons Signature List-of-Signatures)

(define losig0 '(Number))
(define losig1 '(Number String))
(define losig2 '(Number String Boolean))

(define (losig-temp losig)
  (cond [(empty? (rest losig)) (... (sig-temp (first losig)) ...)]
        [(cons? (rest losig)) (... (sig-temp (first losig))
                                   (losig-temp (rest losig)) ...)]))
        

; good? : Signature -> Boolean
; makes sure that every symbol that occurs in the signature belongs to BaseType
(define (good? sig)
  (cond [(symbol? sig) (is-base-type? sig)]
        [(=>? sig) (and (good-losig? (=>-domain sig)) (good? (=>-range sig)))]
        [(union? sig) (good-losig? (union-parts sig))]))

; good-losig? 
(define (good-losig? losig)
  (cond [(empty? (rest losig)) (good? (first losig))]
        [(cons? (rest losig)) (and (good? (first losig))
                                   (good-losig? (rest losig)))]))

; is-base-type : Any -> Boolean
; determines if the given input is a base type
(define (is-base-type? basetype)
  (cond [(symbol=? basetype 'Number) #t]
        [(symbol=? basetype 'String) #t]
        [(symbol=? basetype 'Symbol) #t]
        [(symbol=? basetype 'Boolean) #t]
        [(symbol=? basetype 'Image) #t]
        [else #f]))

(check-expect (good? s0) #t)
(check-expect (good? 'bruh) #f)
(check-expect (good? s1) #t)
(check-expect (good? (make-=> '(Number bruh Boolean) 'Number)) #f)
(check-expect (good? s2) #t)
(check-expect (good? (make-union '(Number String bruh))) #f)


; 3.

(define-struct leaf [info])
(define-struct node [left info right])
; A [BT X] (binary tree over X) is one of:
; -- (make-leaf Symbol)
; -- (make-node [BT X] X [BT X])
; interpretation binary trees with nodes that
; carry some X-kind of value and Symbols as leaves

(define bt0 (make-leaf 'l))
(define bt1 (make-node bt0 "a" bt0))
(define bt2 (make-node bt1 "b" bt1))
(define bt3 (make-node bt2 "c" bt2))

(define (bt-temp bt)
  (cond [(leaf? bt) (... (leaf-info bt) ...)]
        [(node? bt) (... (bt-temp (node-left bt))
                         (node-info bt)
                         (bt-temp (node-right bt)) ...)]))


; [BT String] -> [BT Natural]
; consumes a [BT String] and replaces each String in a node
; with the distance to the “root” of the given tree.
(define (depth bt)
  (cond [(leaf? bt) bt]
        [(node? bt)
         (make-node (add-one-to-all (depth (node-left bt)))
                    0
                    (add-one-to-all (depth (node-right bt))))]))

(define (add-one-to-all bt)
  (cond [(leaf? bt) bt]
        [(node? bt) (make-node (add-one-to-all (node-left bt))
                               (add1 (node-info bt))
                               (add-one-to-all (node-right bt)))]))


         


         
(check-expect (depth bt0) bt0)
(check-expect (depth bt1) (make-node bt0 0 bt0))
(check-expect (depth bt2) (make-node (make-node bt0 1 bt0) 0 (make-node bt0 1 bt0)))


; why do we want accum?
; lost of context
; 

; [BT String] -> [BT Natural]
; consumes a [BT String] and replaces each String in a node
; with the distance to the “root” of the given tree.
#;(define (depth bt)
    (local [;
            ;;
            ; what does it represnet
            ; the current depth, the depth which we have traversed into
          
            ; what is its inital value
            ; 0
          
            ; how does it update
            ; we inc by 1 as we traverse 1 level into tree
          
            ; how do we make use of this
            ; we put that into the tree info
            (define (depth/acc bt acc)
              (cond [(leaf? bt) bt]
                    [(node? bt)
                     (make-node (depth/acc (node-left bt) (add1 acc))
                                acc
                                (depth/acc (node-right bt) (add1 acc)))]))]
      (depth/acc bt 0)))


; 4 ============================

; An LC is one of:
; -- 'cake
; -- (cons 'layer (cons LC '()))
; interpretation an LC represents a layer cake

(define lc0 'cake)
(define lc1 (cons 'layer (cons 'cake '())))
(define lc2 (cons 'layer (cons 'layer (cons 'cake '()))))

; layer-cake : LC -> Natural
(define (layer-cake lc)
  (local [;
          ;
          (define (layer-cake/acc lc count)
            (cond [(symbol? lc) count]
                  [(cons? lc)
                   (if (symbol=? (first lc) 'cake)
                       count
                       (layer-cake/acc (rest lc) (add1 count)))]))]
    (layer-cake/acc lc 0)))

(check-expect (layer-cake lc0) 0)
(check-expect (layer-cake lc1) 1)
(check-expect (layer-cake lc2) 2)

                    














