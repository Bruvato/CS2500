#lang racket
(require 2htdp/universe)
(provide server)
(require rackunit)

;; Mimicking check-satisfied from ISL+, and its opposite (for when the predicate fails)
(define-simple-check (check-unsatisfied pred val)
  (not (pred val)))
(define-simple-check (check-satisfied pred val)
  (pred val))

;; A Matcher is a [SExp -> Boolean] predicate
;; Produces a matcher that recognizes a fixed-length list,
;; whose elements each are matched by the matchers in checks
;; list-check : [List-of Matcher] -> Matcher
(define (list-check checks)
  (λ(val)
    (and (list? val)
         (equal? (length checks) (length val))
         (andmap check checks val))))

;; Produces a matcher that recognizes a value that matches any of the given matchers
;; one-of : Matcher ... -> Matcher
(define (one-of . checks)
  (λ(val) (ormap (λ(c) (check c val)) checks)))

;; Produces a matcher that recognizes a homogeneous, arbitrary-length list
;; whose elements all match the given matcher
;; list-of : Matcher -> Matcher
(define (list-of c)
  (λ(val) (and (list? val)
               (andmap (λ(v) (check c v)) val))))

;; check : Matcher SExp -> Boolean
;; Does the given s-expression match the given matcher
(define (check c v)
  (cond [(or (number? c) (string? c) (symbol? c) (boolean? c))
         (equal? c v)]
        [(list? c) ((list-check c) v)]
        [else (and (procedure? c) (c v))]))

;; Matcher for QBPosn
(define qbposn-check (list 'qbposn natural? natural?))
;; Matcher for Direction
(define dir-check (apply one-of '(UL UR DL DR)))

;; Matcher for Cell
(define cell-check (one-of (list 'countdown natural?)
                           (list 'cycling natural? natural?)))
;; Matcher for Board
(define board-check (list-of (list-of cell-check)))
;; Matcher for Enemy
(define enemy-check (one-of (list 'wanderer qbposn-check)
                            (list 'fixer qbposn-check)))
;; Matcher for [List-of Enemy]
(define enemies-check (list-of enemy-check))
;; Matcher for QBertLevel
(define p1worldstate (list 'world
                           qbposn-check dir-check
                           qbposn-check dir-check
                           natural?
                           board-check
                           enemies-check))
;; Matcher for Player1Message
(define p1message (list 'update p1worldstate))
;; Matcher for Player2Message
(define p2message (one-of (list 'move dir-check)
                          (list 'jump dir-check)))

;; Matcher for arbitrary server messages
(define servermessage (one-of "p1" "p2" p1message p2message))


; is-valid-p1-msg? : SExpr -> Boolean
; Is this a valid message from p1?
(define (is-valid-p1-msg? sexpr)
  (check p1message sexpr))

(define WORLD-SEXP
  '(world (qbposn 3 4) UL
          (qbposn 6 2) DR
          2
          (((countdown 5))
           ((countdown 6) (cycling 3 4)))
          ((wanderer (qbposn 2 2))
           (fixer (qbposn 3 6)))
          ))
(check-satisfied is-valid-p1-msg?
                 `(update ,WORLD-SEXP))
(check-unsatisfied is-valid-p1-msg?
                   '())


; is-valid-p2-msg? : SExpr -> Boolean
; Is this a valid message from p2?
(define (is-valid-p2-msg? sexpr)
  (check p2message sexpr))

(check-satisfied is-valid-p2-msg?
                 '(move UL))
(check-satisfied is-valid-p2-msg?
                 '(jump DR))
(check-unsatisfied is-valid-p2-msg?
                   '(jump DR DR))
(check-unsatisfied is-valid-p2-msg? '())

; is-valid-p2-msg? : SExpr -> Boolean
; Is this a valid message the server can send?
(define (is-valid-server-msg? sexpr)
  (check servermessage sexpr))

(check-satisfied is-valid-server-msg? "p1")
(check-satisfied is-valid-server-msg? "p2")
(check-unsatisfied is-valid-server-msg? "p3")


;; UNIVERSE STATE

;; A 2player-game is a (make-2player-game World [Maybe World])
;; Each 2player-game consists of either one player (waiting for a partner)
;; or two players that will play together.
(define-struct 2player-game [p1 p2])
;; A Universe is a [List-of 2player-game].  All of the 2player-games
;; in the list will have two players, except perhaps the first which
;; might be missing still a second player.

;; ACTUALLY RUNNING THE SERVER
(define (server name)
  (local
    [(define (need-p1-univ? univ)
       (or (empty? univ)
           (iworld? (2player-game-p2 (first univ)))))
     (define (find-game univ wrld)
       (cond [(empty? univ) #f]
             [(cons? univ)
              (if (or (eq? (2player-game-p1 (first univ)) wrld)
                      (eq? (2player-game-p2 (first univ)) wrld))
                  (first univ)
                  (find-game (rest univ) wrld))]))
     (define (add-world univ wrld)
       (cond [(need-p1-univ? univ)
              (make-bundle (cons (make-2player-game wrld #f) univ)
                           (list (make-mail wrld "p1"))
                           '())]
             [else
              (local [(define game1 (first univ))
                      (define p1 (2player-game-p1 game1))]
                (make-bundle (cons (make-2player-game p1 wrld) (rest univ))
                             (list (make-mail wrld "p2"))
                             '()))]))
     (define (relay univ wrld msg)
       (local [(define game (find-game univ wrld))]
         (cond [(false? game)
                (make-bundle univ '() (list wrld))]
               [(eq? (2player-game-p1 game) wrld)
                (p1-msg univ game wrld msg)]
               [else (p2-msg univ game wrld msg)])))
     (define (p1-msg univ game wrld msg)
       (cond [(false? (2player-game-p2 game)) (make-bundle univ '() '())]
             [(is-valid-p1-msg? msg)
              (make-bundle univ
                           (list (make-mail (2player-game-p2 game) msg))
                           '())]
             [else (make-bundle univ
                                (list (make-mail (2player-game-p1 game) '(invalid)))
                                '())]))
     (define (p2-msg univ game wrld msg)
       (if (is-valid-p2-msg? msg)
           (make-bundle univ
                        (list (make-mail (2player-game-p1 game) msg))
                        '())
           (make-bundle univ
                        (list (make-mail (2player-game-p2 game) '(invalid)))
                        '())))]
    (universe '()
              [port 10001]
              [on-new add-world]
              [on-msg relay])))

