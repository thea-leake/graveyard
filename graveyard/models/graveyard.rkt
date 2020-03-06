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
                  filter-not
                  index-of)
         (only-in memoize
                  define/memo
                  memo-lambda)
         (prefix-in b: "board.rkt")
         (prefix-in r: "roles.rkt"))

(provide player-move
         player-flip-location
         location-hidden?
         coords-selected?
         role-at-location
         player-at-location
         player-lost?
         valid-player-turns
         gen-init-turn
         unsafe-move?
         (struct-out turn)
         (struct-out actions))


(struct turn
  (board
   player
   message
   first?
   captured
   src-coords
   valid?)
  #:transparent)

(struct actions
  (available?
   moves
   flips
   captures-thunk)
  #:transparent)


(define (gen-init-turn message)
  (turn (b:gen-board)   ;; board
        "Undecided"     ;; player
        message         ;; message
        #t              ;; first-turn
        ;; Default values not used yet
        r:none-role     ;; captured
        b:none-position ;; selected-coords
        #f              ;; valid? - was move valid?
        ))


(define (piece-at-coordinates coords board)
  (list-ref board
            (b:get-index-from-coordinates coords)))

(define (player-at-location coords board)
  (r:cell-player (piece-at-coordinates coords board)))

(define (piece-belongs-to-player? player coords board)
  (eq? player
       (player-at-location coords board)))

(define (location-empty? coords board)
  (r:cell-empty? (piece-at-coordinates coords
                                  board)))

(define (pieces-in-list piece-list)
  (count (lambda (x)
           (not (r:cell-empty? x)))
         piece-list))

(define (role-at-location coords board)
  (r:cell-role (piece-at-coordinates coords
                                  board)))


(define (is-piece-cannon? coords board)
  (eq? r:cannon
       (role-at-location coords board)))


(define (location-revealed? coords board)
  (r:cell-revealed? (piece-at-coordinates coords board)))


(define (location-hidden? coords board)
  (not (location-revealed? coords board)))


(define (hidden-coordinates board)
  (filter-map (lambda (coords)
                (and (location-hidden? (b:get-coords-from-index coords)
                                       board)
                     (b:get-coords-from-index coords)))
              b:board-indexes))


(define (empty-index-list board)
  (filter (lambda (x)
            (location-revealed? (b:get-coords-from-index x) board))
          b:board-indexes))

(define (empty-coords-list board)
  (map b:get-coords-from-index (empty-index-list board)))


(define (update-coordinates coords piece board)
  (let* ([take-pos (b:get-index-from-coordinates coords)]
         [drop-pos (add1 take-pos)])
    (append (take board take-pos)
            (cons piece
                  (drop board drop-pos)))))


(define (flip-coordinates coords board)
  (update-coordinates coords
                      (r:flip
                       (piece-at-coordinates coords board))
                      board))


(define (empty-coordinates coord board) ;; updates location to empty
  (update-coordinates coord
                      r:none-role
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


(define/memo (hierarchal-able-to-capture? capturing-role defending-role)
  (cond
    ((and (eq? r:leader capturing-role)
          (eq? r:pawn defending-role))
     #f)
    ((and (eq? r:pawn capturing-role)
          (eq? r:leader defending-role))
     #t)
    (else(<= (r:hierarchy-value capturing-role)
             (r:hierarchy-value defending-role)))))


(define/memo (valid-non-cannon-move? src-coords dest-coords)
  (let* ([src-index (b:get-index-from-coordinates src-coords)]
         [dest-index (b:get-index-from-coordinates dest-coords)]
         [location-difference (abs (- src-index
                                      dest-index))])
    (or (= b:board-columns location-difference)
        (and (= 1 location-difference)
             (= (quotient src-index b:board-columns)
                (quotient dest-index b:board-columns))))))


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
    ((< r:cannon-max-pieces piece-count) '(#f "Cannot jump over more than one piece"))
    ((and (= r:cannon-max-pieces piece-count)
          (not (location-empty? dest-coords board)))
     '(#t "Valid cannon move"))
    ((= r:cannon-max-pieces piece-count) '(#f "Cannot jump over more than one piece"))
    ((not (location-empty? dest-coords board))
     '(#f "Cannot capture a piece without jumping over another piece"))
    (else '(#t "Valid move"))))


(define (cannon-row-move-check src-coords dest-coords board)
  (let* ([src-index (b:get-index-from-coordinates src-coords)]
         [dest-index (b:get-index-from-coordinates dest-coords)]
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
                        (- end-index b:board-columns)
                        board
                        (cons (list-ref board end-index)
                              accum)))))


(define (cannon-column-move-check src-coords dest-coords board)
  (let* ([src-index (b:get-index-from-coordinates src-coords)]
         [dest-index (b:get-index-from-coordinates dest-coords)]
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
  (let* ([src-index (b:get-index-from-coordinates src-coords)]
         [dest-index (b:get-index-from-coordinates dest-coords)]
         [move-horizontal? (= (quotient src-index b:board-columns)
                              (quotient dest-index b:board-columns))])
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
      ((b:coords-out-of-range? dest-coords) '(#f "Destination is off of board"))
      ((b:coords-out-of-range? src-coords) '(#f "Source is off of board"))
      ((location-hidden? dest-coords board) '(#f "Cannot capture a hidden piece"))
      ((location-hidden? src-coords board) '(#f "Cannot move a hidden piece"))
      ((not (piece-belongs-to-player? player src-coords board )) '(#f "Cannot move an opponents piece"))
      ((piece-belongs-to-player? player dest-coords board ) '(#f "Cannot capture your own piece"))
      ((is-piece-cannon? src-coords board) (valid-cannon-move? src-coords dest-coords board))
      (else
       (non-cannon-move-check src-coords dest-coords board)))))


(define (valid-moves-for-location state)
  (filter (lambda (dest-coords)
            (car (is-valid-move? state dest-coords)))
          (b:coords-row-columns (turn-src-coords state))))


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

(define (coords-selected? state)
  (not (eq? b:none-position
            (turn-src-coords state))))


(define (player-selectable-coords state)
  (filter (lambda (coords)
            (selectable-coords? state coords))
          b:board-coordinates))


(define (valid-moves-for-player state)
  (filter-map (lambda (coords)
                (let* ([location-check (struct-copy turn state
                                                    [src-coords coords])]
                       [valid-destinations (valid-moves-for-location location-check)])
                  (and (not-null? valid-destinations)
                       (cons coords valid-destinations))))
              (player-selectable-coords state)))


(define (occupied-locations state loc-list)
  (filter-not (lambda (coords)
            (r:cell-empty? (piece-at-coordinates coords
                                                (turn-board state))))
          loc-list))

(define (get-captures state moves)
  (filter-map (lambda (location)
                (let ([capturable-locations
                       (occupied-locations state (cdr location))])
                  (and (not (null? capturable-locations))
                       (cons (car location)
                             capturable-locations))))
              moves))

(define (valid-player-turns state)
  (let* ([moves (valid-moves-for-player state)]
         [flips (hidden-coordinates (turn-board state))])
    (actions (or (not-null? moves)       ;; available?
                 (not-null? flips))
             moves                       ;; moves
             flips                       ;; flips
             (memo-lambda ()
               (get-captures state moves))))) ;; captures-thunk


(define (opponent-move-captures captures dest)
  (and (findf (lambda (x)
                (eq? dest x))
              (cdr captures))
       (car captures)))

(define (opponent-captures state src dest)
  ((actions-captures-thunk
    (valid-player-turns
     (struct-copy turn state
                  [player
                   (r:toggle-player (turn-player state))]
                  [board
                   (move-piece-clobber src
                                       dest
                                       (turn-board state))])))))

(define (unsafe-move? state src dest)
  (let* ([potential-captors
         (filter-map (lambda (captures)
                       (opponent-move-captures captures dest))
                     (opponent-captures state src dest))])
    (if (null? potential-captors)
        #f
        potential-captors)))


(define (player-lost? state)
  (not
   (actions-available? (valid-player-turns state))))

(define (player-flip-location state coords)
  (let ([next-player (if (turn-first? state)
                         (r:toggle-player (player-at-location coords
                                                            (turn-board state)))
                         (r:toggle-player (turn-player state)))])
    (struct-copy turn state
                 [board (flip-coordinates coords
                                          (turn-board state))]
                 [player next-player]
                 [message (role-at-location coords
                                            (turn-board state))]
                 [src-coords b:none-position]
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
                    [player (r:toggle-player (turn-player response))]
                    [board (move-piece-clobber (turn-src-coords response)
                                               dest-coords
                                               (turn-board response))]
                    [captured piece-at-dest]
                    [src-coords b:none-position]))
      (else
       (struct-copy turn response
                    [captured r:none-role])))))
