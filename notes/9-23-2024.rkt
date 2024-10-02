#lang htdp/bsl

; A PS is one of
; - (make-launch ...)
(define-struct launch [foo bar])
; - (make-flight ...)

(define (ps-temp ps)
  (cond [(launch? ps) (... (launch-foo ps) ... (launch-bar ps) ...)]))

; A PS is one of
; - Launch
; - Flight

; A Launch is a (make-launch ...)
; A Flight is a (make-flight ...)



; -------------------------------------

; A Fumble is one of
; - "hi"
; - 0
; - (make-posn 4 Fumble)
; - (cons Fumble #false)

; ONE DATA DEF = ONE TEMP

(define (fumble-temp fumble)
  (cond [(string? fumble) ...] ; atomic
        [(number? fumble) ...] ; atomic
        [(posn? fumble) (... (fumble-temp (posn-y fumble)) ...)] ; posn-x is not nessecary since is atomic
        [(cons? fumble) (... (fumble-temp (first fumble)) ...)]))


; -------------------------------------------------------------------------------

; snat -> Boolean
(define (snat/zero? snat)
  (and (number? snat) (zero? snat)))

(define (my-and b1 b2)
  (cond [b1 b2]
        [else #false]))

