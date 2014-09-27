#lang racket
(require "board.rkt")
(require "moves.rkt")

(define (is-square? sq algn)
  (equal? (algn->coord algn) (square-coord sq)))

(define (contains-piece? sq piece)
  (eq? (square-piece sq) piece))

(define (square-on-board algn board)
  (findf (lambda (x) (is-square? x algn)) board))

(define (remove-piece piece sq)
  (square (square-coord sq) null))

(define (add-piece piece sq)
  (square (square-coord sq) piece))

(define (move-piece piece from to board)
  (map 
   (lambda (sq) 
     (cond
       [(is-square? sq from) (remove-piece piece sq)]
       [(is-square? sq to) (add-piece piece sq)]
       [else sq])) board))

(define (valid-move? move board)
  (define piece (first move))
  (define from (second move))
  (define to (third move))
  (define moves (apply (move-map piece) (list (algn->coord from))))
  (displayln moves)
  (cond 
    [(not (eq? 3 (length move))) #f]
    [(not (contains-piece? (square-on-board from board) piece)) #f]
    [(not (member (algn->coord to) moves)) #f]
    [else #t]))

(define (game-loop board)
  (print-board board)
  (displayln "Enter move (eg. p b2 b4)")
  (define command (read-line))
  (define move (string-split command))
  (cond 
    [(string=? command "exit")  (displayln "goodbye!")]        
    [(valid-move? move board) (game-loop (move-piece (first move) (second move) (third move) board))]
    [else (displayln "not a valid move") (game-loop board)]))

;(define start (game-loop (make-board)))