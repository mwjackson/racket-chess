#lang racket
(require "board.rkt")

(define (on-board? c)
  (cond
    [(null? c) #f]
    [(< (coord-file c) 1) #f]
    [(< (coord-rank c) 1) #f]
    [(> (coord-file c) 8) #f]
    [(> (coord-rank c) 8) #f]
    [else #t]))

(define (legal moves)
  (filter on-board? moves))

(define (pawn-moves c)
  (legal (list 
   (coord (coord-file c) (+ (coord-rank c) 1))
   (cond [(eq? (coord-rank c) 2) (coord (coord-file c) (+ (coord-rank c) 2))]
         [else null]))))

(define (knight-moves c)
  (match c
    [(coord x y) 
     (legal (list (coord (+ x 1) (+ y 2))
      (coord (+ x 2) (+ y 1)) 
      (coord (+ x 2) (- y 1)) 
      (coord (+ x 1) (- y 2)) 
      (coord (- x 1) (- y 2))
      (coord (- x 2) (- y 1))
      (coord (- x 2) (+ y 1))
      (coord (- x 1) (+ y 2))))]))

(define (diagonal-moves c) 
  (let ([up-left (map list (range -1 -8 -1) (range 1 8))]
        [down-left (map list (range -1 -8 -1) (range -1 -8 -1))]
        [up-right (map list (range 1 8) (range 1 8))]
        [down-right (map list (range 1 8) (range -1 -8 -1))]
        [coord-plus-move (lambda (x) (list (+ (coord-file c) (car x)) (+ (coord-rank c) (cadr x))))])
    (map coord-plus-move (append up-left down-left up-right down-right))))

(define (perpendicular-moves c) 
  (let ([up (map list (range 1 8) (make-list 7 0))]
        [down (map list (range -1 -8 -1) (make-list 7 0))]
        [left (map list (make-list 7 0) (range -1 -8 -1))]
        [right (map list (make-list 7 0) (range 1 8))]
        [coord-plus-move (lambda (x) (list (+ (coord-file c) (car x)) (+ (coord-rank c) (cadr x))))])
    (map coord-plus-move (append up down left right))))

(define (bishop-moves c)
  (legal (map pair->coord (diagonal-moves c))))

(define (rook-moves c)
  (legal (map pair->coord (perpendicular-moves c))))

(define (queen-moves c)
  (legal (map pair->coord (append (diagonal-moves c) (perpendicular-moves c)))))

(define (king-moves c)
  (let ([coord-plus-move (lambda (x) (list (+ (coord-file c) (car x)) (+ (coord-rank c) (cadr x))))])
    (legal (map pair->coord (map coord-plus-move (list '(0 1) '(1 0) '(1 1) '(0 -1) '(-1 0) '(-1 -1) '(1 -1) '(-1 1)))))))
