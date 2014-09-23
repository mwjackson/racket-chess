#lang racket
(require "board.rkt")

(provide (all-defined-out))

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

(define (diagonal-moves c) 
  (let ([up-left (map list (range 1 8) (range -1 -8 -1))]
        [down-left (map list (range -1 -8 -1) (range -1 -8 -1))]
        [up-right (map list (range 1 8) (range 1 8))]
        [down-right (map list (range -1 -8 -1) (range 1 8))]
        [coord-plus-move (lambda (x) (list (+ (coord-rank c) (car x)) (+ (coord-file c) (cadr x))))])
    (map coord-plus-move (append up-left down-left up-right down-right))))

(define (perpendicular-moves c) 
  (let ([up (map list (make-list 7 0) (range 1 8))]
        [down (map list (make-list 7 0) (range -1 -8 -1))]
        [left (map list (range -1 -8 -1) (make-list 7 0))]
        [right (map list (range 1 8) (make-list 7 0))]
        [coord-plus-move (lambda (x) (list (+ (coord-rank c) (car x)) (+ (coord-file c) (cadr x))))])
    (map coord-plus-move (append up down left right))))

(define (pawn-moves c)
  (legal (list 
   (coord (+ (coord-rank c) 1) (coord-file c) )
   (cond [(eq? (coord-rank c) 2) (coord (+ (coord-rank c) 2) (coord-file c))]
         [else null]))))

(define (knight-moves c)
  (match c
    [(coord rank file) 
     (legal (list 
             (coord (+ rank 1) (+ file 2))
             (coord (+ rank 2) (+ file 1))
             (coord (+ rank 2) (- file 1)) 
             (coord (+ rank 1) (- file 2)) 
             (coord (- rank 1) (- file 2))
             (coord (- rank 2) (- file 1))
             (coord (- rank 2) (+ file 1))
             (coord (- rank 1) (+ file 2))))]))

(define (bishop-moves c)
  (legal (map pair->coord (diagonal-moves c))))

(define (rook-moves c)
  (legal (map pair->coord (perpendicular-moves c))))

(define (queen-moves c)
  (legal (map pair->coord (append (diagonal-moves c) (perpendicular-moves c)))))

(define (king-moves c)
  (let ([coord-plus-move (lambda (x) (list (+ (coord-rank c) (car x)) (+ (coord-file c) (cadr x))))])
    (legal (map pair->coord (map coord-plus-move (list '(0 1) '(1 0) '(1 1) '(0 -1) '(-1 0) '(-1 -1) '(1 -1) '(-1 1)))))))

(define (move-map piece)
  (match piece
    ["p" pawn-moves]
    ["N" knight-moves]
    ["B" bishop-moves]
    ["R" rook-moves]
    ["Q" queen-moves]
    ["K" king-moves]))
