#lang htdp/isl+

#|


; An Nary Tree is a (make-node Symbol Forest
; A Forest is a [List-of NaryTree]

(define (follow-path t path)
  (cond [(empty? path) t]
        [(cons? path)
         (follow-path (listref (node-kids t) (first path))
                      (rest path))]))

; An NaryBT is one of
; - 'L
; (make-node Symbol NaryBT NaryBT)
(define nbt [value first-kid next-sib])



; ---

(define-struct (nary-node [val kids]))

; 
(define-struct nbt-node [val first-kid next-sib])


(define (convert nt)
  (convert-with-sibling nt 'L))

(define (convert-forest forest)
  (cond [(empty? forest) 'L]
        [(cons? forest)
         (convert-with-sibling (first forest)
                               (convert-forest (rest forest)))]))

; narytree narybt -> narybt
(define (convert-with-sibling nt next-sib)
  (make-nbt-node (naray--nodeval nt)
                 (convert-forest (nary-node-kids nt))
                 next-sib))

|#


; UPPER LIMIT EXAM QUESTION


; =========

; [List-of Link

; A Link is a (make-link anc desc dist)

; A B 1
; B D 1
; A D 2
; B E 1
; A E 2
; A C 1


(define-struct nary-node [val kids])

(define-struct link (anc desc dist))

; nartree -> [List-of Linkk)
(define (convert nt)
  (convert-with-anc nt (nary-node-val nt) 0))


(define (convert-forest lont anc dist-so-far)
  (cond [(empty? lont) '()]
        [(cons? lont)
         (append
          (convert-with-anc (first lont) dist-so-far)
          (convert-forest (rest lont) anc dist-so-far))]))

(define (convert-with-ancn anc dist-so-far)
  (append (list (make-link anc (nary-node-val nt) dist-so-far))
          (convert-forest (nary-node-kids nt) anc (add1 dist-so-far))
          (convert nt)))
                        
  















