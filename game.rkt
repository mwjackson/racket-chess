#lang racket
(require "board.rkt")
(require "moves.rkt")

(define (state) 
  (make-board))

(define (is-square? sq algn)
  (equal? (algn->coord algn) (square-coord sq))) 

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
  (cond 
    [(not (eq? 3 (length move))) #f]
    [(not (member (algn->coord to) moves)) #f]
    [else #t]))

(define (game-loop board)
  (print-board board)
  (displayln "Enter move (eg. p b2 b4)")
  (define command (read-line))
  (define move (string-split command))
  (cond 
    [(string=? command "exit")  (displayln "exited successfully...")]        
    [(valid-move? move board) (game-loop (move-piece (first move) (second move) (third move) board))]
    [else (displayln "could not parse move") (game-loop board)]))

(define start (game-loop (make-board)))