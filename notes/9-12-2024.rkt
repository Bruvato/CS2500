#lang htdp/bsl


(define-struct nested [inside])

; A NesstedString(NS) is one of:
; String
; (make-nested NS)

(define (ns-temp ns)
  (cond [(string? ns ) ...]
        [(nested? ns )...(ns-temp (nested-inside ns)) ...]))

(define NS1 "hello")
(define NS2 (make-nested "hay"))
(define NS3 (make-nested (make-nested "hi")))


(check-expect (replace N1 "bye") "bye")

(define (replace ns s)
  (cond [(string? ns) s]
        [(nested? ns) ((make-nested (replace (nested-inside s))