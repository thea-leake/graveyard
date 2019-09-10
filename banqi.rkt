#lang racket/base
(require racket/list)

(provide (all-defined-out))

(define board-rows 4)
(define board-columns 8)
(define location-count (* board-columns
                          board-rows))

(define player-start-roles
  (flatten
   (list (make-list 1 "General")
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

(define empty-location
  (hash 'empty #t
        'revealed #t
        'role "#Empty#"
        'player #\_))

(define (piece-revealed? piece)
  (hash-ref piece 'revealed))

(define (get-location-attr attr piece [show-hidden? #f])
  (cond
    (show-hidden? (hash-ref piece attr))
    ((piece-revealed? piece)
     (hash-ref piece attr))
    (else "X")))

(define (role-name piece [show-hidden? #f])
  (get-location-attr 'role piece show-hidden?))

(define (player-name piece [show-hidden? #f])
  (get-location-attr 'player piece show-hidden?))


(define (player-roles team)
  (map (lambda (role) (mkpiece team role))
       player-start-roles))

(define (gen-board)
  (shuffle (append (player-roles "Red")
                   (player-roles "Black"))))

(define initial-board
  (gen-board))

(define (flip piece)
  (hash-set piece 'revealed #t))

(define (x-pos coords)
  (car coords))

(define (y-pos coords)
  (cadr coords))

(define (get-index-from-coordinates coords)
  (let ([x (x-pos coords)]
        [y (y-pos coords)])
    (+ (* y board-columns)
       x)))

(define (get-coords-from-index index)
  (let ([x (remainder index
                      board-columns)]
        [y (quotient index
                     board-columns)])
    (list x y)))

(define (get-row index board)
  (let* ([start (* index board-columns)]
         [end (+ start board-columns)])
    (drop (take board
                end)
          start)))

(define (index-in-range? index)
  (and (<= 0 index)
       (> location-count index)))

(define (coords-in-range? coords)
  (index-in-range? (get-index-from-coordinates coords)))

(define (coords-out-of-range? coords)
  (not (coords-in-range? coords)))

(define (piece-at-coordinates coords board)
  (list-ref board
            (get-index-from-coordinates coords)))

(define (player-at-location coords board)
  (hash-ref (piece-at-coordinates coords
                                  board)
            'player))

(define (piece-belongs-to-player? player coords board)
  (eq? player
      (player-at-location coords board)))

(define (role-at-location coords board)
  (hash-ref (piece-at-coordinates coords
                                  board)
            'role))

(define (is-piece-cannon? coords board)
  (eq? "Cannon"
       (role-at-location coords board)))

(define (location-revealed? coords board)
  (piece-revealed? (piece-at-coordinates coords board)))

(define (empty-index-list board)
  (filter (lambda (x)
            (location-revealed? (get-coords-from-index x) board))
          (range location-count)))

(define (empty-coords-list board)
  (map get-coords-from-index (empty-index-list board)))

(define (shift-location-coords src-coords direction count)
  (let ([x (x-pos src-coords)]
        [y (y-pos src-coords)])
    (cond
      ((eq? direction 'up) (list x (+ count y)))
      ((eq? direction 'down) (list x (- count y)))
      ((eq? direction 'left) (list (- count x) y))
      ((eq? direction 'right) (list (+ count x) y)))))

(define (update-coordinates coords piece board)
  (let* ([take-pos (get-index-from-coordinates coords)]
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
                      empty-location
                      board))

(define (move-piece-clobber src-coords dest-coords board)
    (empty-coordinates src-coords
                       (update-coordinates dest-coords
                                           (piece-at-coordinates src-coords
                                                                 board)
                                           board)
                                           ))

(define (valid-non-cannon-move? src-coords dest-coords)
  (let* ([src-index (get-index-from-coordinates src-coords)]
         [dest-index (get-index-from-coordinates dest-coords)]
         [location-difference (abs (- src-index
                                      dest-index))])
    (or (= board-columns location-difference)
        (and (= 1 location-difference)
             (= (remainder src-index board-columns)
                (remainder src-index board-columns))))))

(define (non-cannon-move-check src-coords dest-coords)
  (cond
    ((valid-non-cannon-move? src-coords dest-coords)
     '(#t "Valid move."))
    (else '(#f "Invalid Invalid move location"))))

(define (valid-cannon-move? src-coords dest-coords board)
  (cond
    (else '(#t "Valid move"))))

(define (is-valid-move? player src-coords dest-coords board)
  (cond
    ((coords-out-of-range? dest-coords) '(#f "Destination is off of board"))
    ((coords-out-of-range? src-coords) '(#f "Source is off of board"))
    ((piece-belongs-to-player? player dest-coords board ) '(#f "Cannot capture your own piece"))
    ((is-piece-cannon? src-coords board) (valid-cannon-move? src-coords dest-coords board))
    (else
     (non-cannon-move-check src-coords dest-coords))))
