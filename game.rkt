#lang racket
(require "board.rkt" "moves.rkt")

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

(define (valid-move? move)
  (let [m (string-split move)]
    (cond 
      [(length m 3) m]
      [else #f])))

(define (game-loop board)
  (print-board board)
  (display "Enter move (eg. p b2 b4)")
  (define command (read-line))
  (let [m (string-split move)]
    (cond 
      [(string=? command "exit")  (displayln "exited successfully...")]        
      [(valid-move? command) (game-loop (move-piece (first m) (second m) (third m) board))]
      [else (displayln "could not parse move") (game-loop board)])))