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

(define (location-revealed? piece)
  (hash-ref piece 'revealed))

(define (get-location-attr attr piece [show-hidden? #f])
  (cond
    (show-hidden? (hash-ref piece attr))
    ((location-revealed? piece)
     (hash-ref piece attr))
    (else "X")))

(define (role-name piece [show-hidden? #f])
  (get-location-attr 'role piece show-hidden?))

(define (player-name piece [show-hidden? #f])
  (get-location-attr 'player piece show-hidden?))


(define (player-roles team)
  (map (lambda (role) (mkpiece team role))
       player-start-roles))


(define initial-board
  (shuffle (append (player-roles "Red")
                   (player-roles "Black"))))

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

(define (update-coordinates coords piece board)
  (let* ([take-pos (get-index-coordinates coords)]
         [drop-pos (+ 1 take-pos)])
    (append (take board take-pos)
            (cons piece
                  (drop board drop-pos)))))

(define (flip-coordinates coords board)
  (update-coordinates coords
                      (flip
                       (piece-at-coordinates coords board))
                      board))

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

