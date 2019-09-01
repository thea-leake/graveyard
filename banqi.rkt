#lang racket/base
(require racket/list)

(provide (all-defined-out))

(define board-rows 4)
(define board-columns 8)

(define player-start-roles
  (flatten
   (list (make-list 2 "General")
         (make-list 2 "Advisor")
         (make-list 2 "Elephant")
         (make-list 2 "Chariot")
         (make-list 2 "Horse")
         (make-list 5 "Soldier")
         (make-list 2 "Cannon"))))



(define (mkpiece player role)
  (hash 'player player
        'revealed #f
        'role role
        'empty #f))

(define empty-pos
  (hash 'empty #t
        'revealed #t
        'role "#Empty#"
        'player #\_))

(define (role-name piece)
  (hash-ref piece 'role))

(define (player-name piece)
  (hash-ref piece 'player))

(define (location-revealed? piece)
  (hash-ref piece 'revealed))

(define player1-roles
  (map (lambda (role) (mkpiece 1 role))
       player-start-roles))

(define player2-roles
  (map (lambda (role) (mkpiece 2 role))
       player-start-roles))

(define initial-board
  (shuffle (append player1-roles
                   player2-roles)))

(define (flip piece)
  (hash-set piece 'revealed #t))

(define (x-pos coords)
  (car coords))

(define (y-pos coords)
  (cadr coords))

(define (get-index-coordinates coords)
  (let ([x (x-pos coords)]
        [y (y-pos coords)])
    (+ (* y board-columns)
       x)))

(define (get-coords-from-index index)
  (let ([x (quotient index
                     board-columns)]
        [y (remainder index
                      board-columns)])
    (list x y)))

(define (get-row index board)
  (let* ([start (* index board-columns)]
         [end (+ start board-columns)])
    (drop (take board
                end)
          start)))

(define (piece-at-coordinates coords board)
  (list-ref board
            (get-index-coordinates coords)))

(define (update-coordinates coord piece board)
  (let* ([take-pos (get-index-coordinates coord)]
         [drop-pos (+ 1 take-pos)])
    (append (take board take-pos)
            (cons piece
                  (drop board drop-pos)))))

(define (empty-coordinates coord board) ;; updates location to empty
  (update-coordinates coord
                      empty-pos
                      board))

(define (move-piece-clobber src-coords dest-coords board)
    (empty-coordinates src-coords
                       (update-coordinates dest-coords
                                           (piece-at-coordinates src-coords
                                                                 board)
                                           board)
                                           ))

