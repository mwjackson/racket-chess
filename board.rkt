#lang racket
(require unstable/list)

(provide (all-defined-out))

(struct coord (rank file) #:transparent
  #:property prop:custom-write (lambda (c port w?) (write (print-coord c) port)))

(struct square (coord [piece #:mutable])
  #:property prop:custom-write (lambda (s port w?) (write (print-square s) port)))

(define (print-coord c)
  (format "~a~a" (to-file (coord-file c)) (coord-rank c)))

(define (print-square s)
  (let ([c (square-coord s)])
    (cond
      [(null? (square-piece s)) (format "~a~a" (to-file (coord-file c)) (coord-rank c))]
      [else (format "~a" (square-piece s))])))

(define (empty-board)
    (reverse (map pair->coord (cartesian-product (range 1 9) (range 1 9)))))

(define (pieces)
  (append 
  (list 'R 'N 'B 'K 'Q 'B 'N 'R)
  (list 'p 'p 'p 'p 'p 'p 'p 'p)
  (make-list 8 null)
  (make-list 8 null)
  (make-list 8 null)
  (make-list 8 null)
  (list 'p 'p 'p 'p 'p 'p 'p 'p)
  (list 'R 'N 'B 'K 'Q 'B 'N 'R)))

(define (make-board)
  (map (lambda (sq p) (square sq p)) (empty-board) (pieces))) 

(define (print-board squares)
  (define (print-board-iter squares)
    (unless (empty? squares)
      (displayln (reverse (take squares 8)))
      (print-board-iter (drop squares 8))))
  (print-board-iter squares))

(define (to-file x)
  (match x
    [1 "a"]
    [2 "b"]
    [3 "c"]
    [4 "d"]
    [5 "e"]
    [6 "f"]
    [7 "g"]
    [8 "h"]))

(define (from-file f)
  (match f
    [#\a 1]
    [#\b 2]
    [#\c 3]
    [#\d 4]
    [#\e 5]
    [#\f 6]
    [#\g 7]
    [#\h 8]))

(define (pair->coord pr)
  (coord (car pr) (cadr pr)))

(define (algn->coord notation)
  (let ([chars (string->list notation)])
    (coord (string->number (string (cadr chars))) (from-file (car chars)))))

