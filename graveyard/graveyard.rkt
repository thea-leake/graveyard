;; Copyright 2019 Thea Leake

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;; http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

#lang racket/base
(require (only-in racket/list
                  take
                  drop
                  range
                  flatten
                  make-list
                  shuffle
                  count
                  filter-map
                  index-of)
         (only-in memoize
                  define/memo))

(provide board-coordinates
         board-columns
         board-rows
         gen-board
         role-name
         player-name
         get-row
         piece-revealed?
         piece-empty?
         player-move
         player-flip-location
         location-hidden?
         flip-coordinates
         toggle-player
         role-at-location
         player-at-location
         player-lost?
         gen-init-turn
         (struct-out turn))


(struct turn
  (board
   player
   message
   first?
   captured
   src-coords
   valid?))

(define board-rows 4)
(define board-columns 8)
(define location-count (* board-columns
                          board-rows))


(define (gen-init-turn message)
  (turn (gen-board) ;; board
        "Undecided" ;; player
        message     ;; message
        #t          ;; first-turn
        ;; Default values not used yet
        #f  ;; captured
        #f  ;; selected-coords
        #f  ;; valid? - was move valid?
        ))


(define (x-pos coords)
  (car coords))

(define (y-pos coords)
  (cadr coords))

(define/memo (get-index-from-coordinates coords)
  (let ([x (x-pos coords)]
        [y (y-pos coords)])
    (+ (* y board-columns)
       x)))


(define/memo (get-coords-from-index index)
  (let ([x (remainder index
                      board-columns)]
        [y (quotient index
                     board-columns)])
    (list x y)))


(define board-indexes (range location-count))
(define board-coordinates (map get-coords-from-index board-indexes))

;; number of pieces that can be involve in a cannon move
;; cannon, piece cannon jumps over, piece cannon takes
(define cannon-max-pieces 3)


;; roles
(define leader "Lich")
(define advisor "Vampire")
(define elephant "Zombie")
(define chariot "Ghoul")
(define horse "Skeleton")
(define cannon "Wraith")
(define pawn "Skeleton")


;; it is worth noting that this is not referenced when the cannon is capturing
;; cannons can capture any unit, and any unit except soldier can capture the cannon
(define role-hierarchy
  (list leader advisor elephant chariot horse cannon pawn "#Empty#"))


(define player-start-roles
  (flatten
   (list (make-list 1 leader)
         (make-list 2 advisor)
         (make-list 2 elephant)
         (make-list 2 chariot)
         (make-list 2 horse)
         (make-list 5 pawn)
         (make-list 2 cannon))))

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

(define (piece-empty? piece)
  (hash-ref piece 'empty))

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


(define (toggle-player player)
  (if (eq? player "Red")
      "Black"
      "Red"))

(define (flip piece)
  (hash-set piece 'revealed #t))



(define/memo (get-row index board)
  (let* ([start (* index board-columns)]
         [end (+ start board-columns)])
    (drop (take board
                end)
          start)))


(define/memo (index-in-range? index)
  (and (<= 0 index)
       (> location-count index)))


(define/memo (coords-in-range? coords)
  (index-in-range? (get-index-from-coordinates coords)))


(define/memo (coords-out-of-range? coords)
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

(define (location-empty? coords board)
  (hash-ref (piece-at-coordinates coords
                                  board)
            'empty))

(define (pieces-in-list piece-list)
  (count (lambda (x)
           (not (hash-ref x
                          'empty)))
         piece-list))

(define (role-at-location coords board)
  (hash-ref (piece-at-coordinates coords
                                  board)
            'role))


(define (is-piece-cannon? coords board)
  (eq? cannon
       (role-at-location coords board)))


(define (location-revealed? coords board)
  (piece-revealed? (piece-at-coordinates coords board)))


(define (location-hidden? coords board)
  (not (location-revealed? coords board)))


(define (hidden-coordinates board)
  (filter-map (lambda (coords)
                (and (location-hidden? (get-coords-from-index coords)
                                       board)
                     (get-coords-from-index coords)))
              board-indexes))


(define (empty-index-list board)
  (filter (lambda (x)
            (location-revealed? (get-coords-from-index x) board))
          board-indexes))

(define (empty-coords-list board)
  (map get-coords-from-index (empty-index-list board)))


(define (update-coordinates coords piece board)
  (let* ([take-pos (get-index-from-coordinates coords)]
         [drop-pos (add1 take-pos)])
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
                                         board)))


(define (move-piece src-coords dest-coords board)
  (let ([captured-piece (piece-at-coordinates src-coords board)]
        [updated-board (move-piece-clobber src-coords
                                           dest-coords
                                           board)])
    (list captured-piece updated-board)))

(define/memo (hierarchy-value role)
  (index-of role-hierarchy role))


(define/memo (hierarchal-able-to-capture? capturing-role defending-role)
  (cond
    ((and (eq? leader capturing-role)
          (eq? pawn defending-role))
     #f)
    ((and (eq? pawn capturing-role)
          (eq? leader defending-role))
     #t)
    (else(<= (hierarchy-value capturing-role)
             (hierarchy-value defending-role)))))


(define/memo (valid-non-cannon-move? src-coords dest-coords)
  (let* ([src-index (get-index-from-coordinates src-coords)]
         [dest-index (get-index-from-coordinates dest-coords)]
         [location-difference (abs (- src-index
                                      dest-index))])
    (or (= board-columns location-difference)
        (and (= 1 location-difference)
             (= (quotient src-index board-columns)
                (quotient dest-index board-columns))))))


(define (non-cannon-move-check src-coords dest-coords board)
  (let* ([role? (lambda (x) (role-at-location x board))]
         [capturable? (hierarchal-able-to-capture? (role? src-coords)
                                                   (role? dest-coords))])
    (cond
      ((not capturable?) '(#f "Target piece too powerful to capture"))
      ((valid-non-cannon-move? src-coords dest-coords)
       '(#t "Valid move."))
      (else '(#f "Invalid move location")))))


(define (cannon-move-list-check piece-count dest-coords board)
  (cond
    ((>= 1 piece-count) '(#f "Must jump over one piece"))
    ((< cannon-max-pieces piece-count) '(#f "Cannot jump over more than one piece"))
    ((and (= cannon-max-pieces piece-count)
          (not (location-empty? dest-coords board)))
     '(#t "Valid cannon move"))
    ((= cannon-max-pieces piece-count) '(#f "Cannot jump over more than one piece"))
    ((not (location-empty? dest-coords board))
     '(#f "Cannot capture a piece without jumping over another piece"))
    (else '(#t "Valid move"))))


(define (cannon-row-move-check src-coords dest-coords board)
  (let* ([src-index (get-index-from-coordinates src-coords)]
         [dest-index (get-index-from-coordinates dest-coords)]
         [first-index (min src-index
                           dest-index)]
         [last-index (max src-index
                          dest-index)])
    (cannon-move-list-check (pieces-in-list (drop (take board
                                                        (add1 last-index))
                                                  first-index))
                            dest-coords
                            board)))


(define (column-range-list start-index end-index board accum)
  (cond
    ((> start-index end-index) accum)
    (else
     (column-range-list start-index
                        (- end-index board-columns)
                        board
                        (cons (list-ref board end-index)
                              accum)))))


(define (cannon-column-move-check src-coords dest-coords board)
  (let* ([src-index (get-index-from-coordinates src-coords)]
         [dest-index (get-index-from-coordinates dest-coords)]
         [first-index (min src-index dest-index)]
         [last-index (max src-index dest-index)]
         [column-list (column-range-list first-index
                                         last-index
                                         board
                                         '())])
    (cannon-move-list-check (pieces-in-list column-list)
                            dest-coords
                            board)))


(define (valid-cannon-move? src-coords dest-coords board)
  (let* ([src-index (get-index-from-coordinates src-coords)]
         [dest-index (get-index-from-coordinates dest-coords)]
         [move-horizontal? (= (quotient src-index board-columns)
                              (quotient dest-index board-columns))])
    (cond
      (move-horizontal? (cannon-row-move-check src-coords
                                               dest-coords
                                               board))
      (else (cannon-column-move-check src-coords
                                      dest-coords
                                      board)))))



(define (is-valid-move? state dest-coords)
  (let ([board (turn-board state)]
        [src-coords (turn-src-coords state)]
        [player (turn-player state)])
    (cond
      ((coords-out-of-range? dest-coords) '(#f "Destination is off of board"))
      ((coords-out-of-range? src-coords) '(#f "Source is off of board"))
      ((location-hidden? dest-coords board) '(#f "Cannot capture a hidden piece"))
      ((location-hidden? src-coords board) '(#f "Cannot move a hidden piece"))
      ((not (piece-belongs-to-player? player src-coords board )) '(#f "Cannot move an opponents piece"))
      ((piece-belongs-to-player? player dest-coords board ) '(#f "Cannot capture your own piece"))
      ((is-piece-cannon? src-coords board) (valid-cannon-move? src-coords dest-coords board))
      (else
       (non-cannon-move-check src-coords dest-coords board)))))


(define/memo (coords-row-columns coords)
  (let ([check (lambda (fn check-coords)
                 (= (fn check-coords)
                    (fn coords)))])
    (filter (lambda (check-coords)
              (or (check x-pos check-coords)
                  (check y-pos check-coords)))
            board-coordinates)))


(define (valid-moves-for-location state)
  (filter (lambda (dest-coords)
            (car (is-valid-move? state dest-coords)))
          (coords-row-columns (turn-src-coords state))))


(define (not-null? lst)
  (not (null? lst)))


(define (selectable-coords? state coords)
  (let ([board (turn-board state)])
    (and (not (location-empty? coords
                               board))
         (location-revealed? coords
                             board)
         (piece-belongs-to-player? (turn-player state)
                                   coords
                                   board)
         coords)))


(define (player-selectable-coords state)
  (filter (lambda (coords)
            (selectable-coords? state coords))
          board-coordinates))


(define (valid-moves-for-player state)
  (filter-map (lambda (coords)
                (let* ([location-check (struct-copy turn state
                                                    [src-coords coords])]
                       [valid-destinations (valid-moves-for-location location-check)])
                  (and (not-null? valid-destinations)
                       (cons coords valid-destinations))))
              (player-selectable-coords state)))


(define (valid-player-turns state)
  (let* ([moves (valid-moves-for-player state)]
         [flips (hidden-coordinates (turn-board state))])
    (hash 'moves (make-immutable-hash moves )
          'flips flips
          'actions-available? (or (not-null? moves)
                                  (not-null? flips)))))

(define (player-lost? state)
  (not
   (hash-ref (valid-player-turns state)
             'actions-available?)))

(define (player-flip-location state coords)
  (let ([next-player (if (turn-first? state)
                         (toggle-player (player-at-location coords
                                                            (turn-board state)))
                         (toggle-player (turn-player state)))])
    (struct-copy turn state
                 [board (flip-coordinates coords
                                          (turn-board state))]
                 [player next-player]
                 [message (role-at-location coords
                                            (turn-board state))]
                 [src-coords #f]
                 [first? #f])))


(define (player-move state dest-coords)
  (let* ([piece-at-dest (piece-at-coordinates dest-coords (turn-board state))]
         [move-check (is-valid-move? state
                                     dest-coords)]
         [response (struct-copy turn state
                                [valid? (car move-check) ]
                                [message (cadr move-check)])])
    (cond
      ((turn-valid? response)
       (struct-copy turn response
                    [player (toggle-player (turn-player response))]
                    [board (move-piece-clobber (turn-src-coords response)
                                               dest-coords
                                               (turn-board response))]
                    [captured piece-at-dest]))
      (else
       (struct-copy turn response
                    [captured empty-location])))))