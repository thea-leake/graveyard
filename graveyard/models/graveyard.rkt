;; Copyright 2019-2021 Thea Leake

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;; http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

#lang typed/racket/base


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


(require (only-in tmemoize
                  memoized
                  memoize)
         (prefix-in b: "board.rkt")
         (prefix-in r: "roles.rkt"))

(require/typed racket/list
  [take (-> (Listof r:cell) b:Index (Listof r:cell))]
  [drop (-> (Listof r:cell) b:Index (Listof r:cell))]
  [count (-> (-> r:cell Boolean)
             (Listof r:cell)
             Integer)]
  [filter-map (All (in out)
                   (-> (-> in (U out False))
                       (Listof in)
                       (Listof out)))]
  [filter-not (All (in)
                   (-> (-> in Boolean)
                       (Listof in)
                       (Listof in)))])

(struct turn
  ([board : (Listof r:cell)]
   [player : r:Player]
   [first-player : r:Player]
   [message : String]
   [first? : Boolean]
   [captured : (U r:cell False)]
   [src-coords : b:Position]
   [valid? : Boolean])
  #:transparent)

(struct actions
  ([available? : Boolean]
   [moves : (Listof (Listof b:Position))]
   [flips : (Listof b:Position)]
   [captures-thunk : (-> (Listof (Listof b:Position)))])
  #:transparent)


(: gen-init-turn (-> String turn))
(define (gen-init-turn message)
  (turn (b:gen-board)   ;; board
        "Undecided"     ;; player
        "Undecided"     ;; player
        message         ;; message
        #t              ;; first-turn
        ;; Default values not used yet
        r:none-role     ;; captured
        b:none-position ;; selected-coords
        #f              ;; valid? - was move valid?
        ))



(: piece-at-coordinates
   (-> b:Position (Listof r:cell) r:cell))
(define (piece-at-coordinates coords board)
  (list-ref board
            (b:get-index-from-coordinates coords)))


(: player-at-location (-> b:Position (Listof r:cell) r:Player))
(define (player-at-location coords board)
  (r:cell-player (piece-at-coordinates coords board)))


(: piece-belongs-to-player?
   (-> r:Player b:Position (Listof r:cell) Boolean))
(define (piece-belongs-to-player? player coords board)
  (eq? player
       (player-at-location coords board)))

(: location-empty? (-> b:Position (Listof r:cell) Boolean))
(define (location-empty? coords board)
  (r:cell-empty? (piece-at-coordinates coords
                                  board)))

(: pieces-in-list (-> (Listof r:cell) Integer))
(define (pieces-in-list piece-list)
  (count (lambda ([piece : r:cell])
           (not (r:cell-empty? piece)))
         piece-list))


(: role-at-location (-> b:Position (Listof r:cell) r:Role))
(define (role-at-location coords board)
  (r:cell-role (piece-at-coordinates coords
                                  board)))


(: is-piece-cannon? (-> b:Position (Listof r:cell) Boolean))
(define (is-piece-cannon? coords board)
  (eq? r:cannon
       (role-at-location coords board)))


(: location-revealed? (-> b:Position (Listof r:cell) Boolean))
(define (location-revealed? coords board)
  (r:cell-revealed? (piece-at-coordinates coords board)))


(: location-hidden? (-> b:Position (Listof r:cell) Boolean))
(define (location-hidden? coords board)
  (not (location-revealed? coords board)))


(: hidden-coordinates (-> (Listof r:cell) (Listof b:Position)))
(define (hidden-coordinates board)
  (filter-map (lambda ([index : b:Index])
                (and (location-hidden? (b:get-coords-from-index index)
                                       board)
                     (b:get-coords-from-index index)))
              b:board-indexes))


(: empty-index-list (-> (Listof r:cell) (Listof b:Index)))
(define (empty-index-list board)
  (filter (lambda ([index : b:Index])
            (location-revealed? (b:get-coords-from-index index) board))
          b:board-indexes))


(: empty-coords-list (-> (Listof r:cell) (Listof b:Position)))
(define (empty-coords-list board)
  (map b:get-coords-from-index (empty-index-list board)))


(: update-coordinates
   (-> b:Position r:cell (Listof r:cell) (Listof r:cell)))
(define (update-coordinates coords piece board)
  (let* ([take-pos (b:get-index-from-coordinates coords)]
         [drop-pos (cast (add1 take-pos)
                         b:Index)])
    (append (take board take-pos)
            (cons piece
                  (drop board drop-pos)))))


(: flip-coordinates (-> b:Position (Listof r:cell) (Listof r:cell)))
(define (flip-coordinates coords board)
  (update-coordinates coords
                      (r:flip
                       (piece-at-coordinates coords board))
                      board))


(: empty-coordinates (-> b:Position (Listof r:cell) (Listof r:cell)))
(define (empty-coordinates coord board) ;; updates location to empty
  (update-coordinates coord
                      r:none-role
                      board))


(: move-piece-clobber
   (-> b:Position b:Position (Listof r:cell) (Listof r:cell)))
(define (move-piece-clobber src-coords dest-coords board)
  (empty-coordinates src-coords
                     (update-coordinates dest-coords
                                         (piece-at-coordinates src-coords
                                                               board)
                                         board)))


(: move-piece
   (-> b:Position b:Position (Listof r:cell) (List r:cell (Listof r:cell))))
(define (move-piece src-coords dest-coords board)
  (let ([captured-piece (piece-at-coordinates src-coords board)]
        [updated-board (move-piece-clobber src-coords
                                           dest-coords
                                           board)])
    (list captured-piece updated-board)))


(memoized
 (: hierarchal-able-to-capture? (-> r:Role r:Role Boolean))
 (define (hierarchal-able-to-capture? capturing-role defending-role)
   (cond
     ((and (eq? r:leader capturing-role)
           (eq? r:pawn defending-role))
      #f)
     ((and (eq? r:pawn capturing-role)
           (eq? r:leader defending-role))
      #t)
     (else(<= (r:hierarchy-value capturing-role)
              (r:hierarchy-value defending-role))))))


(memoized
 (: valid-non-cannon-move? (-> b:Position b:Position Boolean))
 (define (valid-non-cannon-move? src-coords dest-coords)
   (let* ([src-index (b:get-index-from-coordinates src-coords)]
          [dest-index (b:get-index-from-coordinates dest-coords)]
          [location-difference (abs (- src-index
                                       dest-index))])
     (or (= b:board-columns location-difference)
         (and (= 1 location-difference)
              (= (quotient src-index b:board-columns)
                 (quotient dest-index b:board-columns)))))))


(: non-cannon-move-check
   (-> b:Position b:Position (Listof r:cell) (Pairof Boolean String)))
(define (non-cannon-move-check src-coords dest-coords board)
  (let* ([role? (lambda ([coords : b:Position])
                  (role-at-location coords board))]
         [capturable? (hierarchal-able-to-capture? (role? src-coords)
                                                   (role? dest-coords))])
    (cond
      ((not capturable?) '(#f . "Target piece too powerful to capture"))
      ((valid-non-cannon-move? src-coords dest-coords)
       '(#t . "Valid move."))
      (else '(#f . "Invalid move location")))))


(: cannon-move-list-check
   (-> Integer b:Position (Listof r:cell) (Pairof Boolean String)))
(define (cannon-move-list-check piece-count dest-coords board)
  (cond
    ((>= 1 piece-count) '(#f . "Must jump over one piece"))
    ((< r:cannon-max-pieces piece-count) '(#f . "Cannot jump over more than one piece"))
    ((and (= r:cannon-max-pieces piece-count)
          (not (location-empty? dest-coords board)))
     '(#t . "Valid cannon move"))
    ((= r:cannon-max-pieces piece-count) '(#f . "Cannot jump over more than one piece"))
    ((not (location-empty? dest-coords board))
     '(#f . "Cannot capture a piece without jumping over another piece"))
    (else '(#t . "Valid move"))))


(: cannon-row-move-check
   (-> b:Position b:Position (Listof r:cell) (Pairof Boolean String)))
(define (cannon-row-move-check src-coords dest-coords board)
  (let* ([src-index (b:get-index-from-coordinates src-coords)]
         [dest-index (b:get-index-from-coordinates dest-coords)]
         [first-index (cast (min src-index
                                 dest-index)
                            b:Index)]
         [last-index (cast (max src-index
                                dest-index)
                           b:Index)])
    (cannon-move-list-check (pieces-in-list (drop (take board
                                                        (cast (add1 last-index)
                                                              b:Index))
                                                  first-index))
                            dest-coords
                            board)))


(: column-range-list
   (-> b:Index Integer (Listof r:cell) (Listof r:cell) (Listof r:cell)))
(define (column-range-list start-index end-index board accum)
  (cond
    ((> start-index end-index) accum)
    (else
     (column-range-list start-index
                        (- end-index b:board-columns)
                        board
                        (cons (list-ref board end-index)
                              accum)))))


(: cannon-column-move-check
   (-> b:Position b:Position (Listof r:cell) (Pairof Boolean String)))
(define (cannon-column-move-check src-coords dest-coords board)
  (let* ([src-index (b:get-index-from-coordinates src-coords)]
         [dest-index (b:get-index-from-coordinates dest-coords)]
         [first-index (cast (min src-index dest-index)
                            b:Index)]
         [last-index (cast (max src-index dest-index)
                           b:Index)]
         [column-list (column-range-list first-index
                                         last-index
                                         board
                                         '())])
    (cannon-move-list-check (pieces-in-list column-list)
                            dest-coords
                            board)))



(: valid-cannon-move?
   (-> b:Position b:Position (Listof r:cell) (Pairof Boolean String)))
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


(: is-valid-move? (-> turn b:Position (Pairof Boolean String)))
(define (is-valid-move? state dest-coords)
  (let ([board (turn-board state)]
        [src-coords (turn-src-coords state)]
        [player (turn-player state)])
    (cond
      ((b:coords-out-of-range? dest-coords) '(#f . "Destination is off of board"))
      ((b:coords-out-of-range? src-coords) '(#f . "Source is off of board"))
      ((location-hidden? dest-coords board) '(#f . "Cannot capture a hidden piece"))
      ((location-hidden? src-coords board) '(#f . "Cannot move a hidden piece"))
      ((not (piece-belongs-to-player? player src-coords board )) '(#f . "Cannot move an opponents piece"))
      ((piece-belongs-to-player? player dest-coords board ) '(#f . "Cannot capture your own piece"))
      ((is-piece-cannon? src-coords board) (valid-cannon-move? src-coords dest-coords board))
      (else
       (non-cannon-move-check src-coords dest-coords board)))))


(: valid-moves-for-location (-> turn (Listof b:Position)))
(define (valid-moves-for-location state)
  (filter (lambda ([dest-coords : b:Position])
            (car (is-valid-move? state dest-coords)))
          (b:coords-row-columns (turn-src-coords state))))


(: not-null? (-> (Listof Any) Boolean))
(define (not-null? lst)
  (not (null? lst)))


(: selectable-coords? (-> turn b:Position (U b:Position Boolean)))
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


(: coords-selected? (-> turn Boolean))
(define (coords-selected? state)
  (not (eq? b:none-position
            (turn-src-coords state))))


(: player-selectable-coords (-> turn (Listof b:Position)))
(define (player-selectable-coords state)
  (filter (lambda ([coords : b:Position])
            (selectable-coords? state coords))
          b:board-coordinates))


(: valid-moves-for-player (-> turn (Listof (Listof b:Position))))
(define (valid-moves-for-player state)
  (filter-map (lambda ([coords : b:Position])
                (let* ([location-check (struct-copy turn state
                                                    [src-coords coords])]
                       [valid-destinations (valid-moves-for-location location-check)])
                  (and (not-null? valid-destinations)
                       (cons coords valid-destinations))))
              (player-selectable-coords state)))


(: occupied-locations
   (-> turn (Listof b:Position) (Listof b:Position)))
(define (occupied-locations state loc-list)
  (filter-not (lambda ([coords : b:Position])
            (r:cell-empty? (piece-at-coordinates coords
                                                (turn-board state))))
          loc-list))


(: get-captures
   (-> turn (Listof (Listof b:Position)) (Listof (Listof b:Position))))
(define (get-captures state moves)
  (filter-map (lambda ([location : (Listof b:Position)])
                (let ([capturable-locations
                       (occupied-locations state (cdr location))])
                  (and (not (null? capturable-locations))
                       (cons (car location)
                             capturable-locations))))
              moves))


(: valid-player-turns (-> turn actions))
(define (valid-player-turns state)
  (let* ([moves (valid-moves-for-player state)]
         [flips (hidden-coordinates (turn-board state))])
    (actions (or (not-null? moves)       ;; available?
                 (not-null? flips))
             moves                       ;; moves
             flips                       ;; flips
             (memoize
              (lambda ()
               (get-captures state moves)))))) ;; captures-thunk



(: opponent-move-captures
   (-> (Listof b:Position) b:Position (U b:Position False)))
(define (opponent-move-captures captures dest)
  (and (findf (lambda (x)
                (eq? dest x))
              (cdr captures))
       (car captures)))


(: opponent-captures
   (-> turn b:Position b:Position (Listof (Listof b:Position))))
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


(: unsafe-move?
   (-> turn b:Position b:Position (U (Listof b:Position) False)))
(define (unsafe-move? state src dest)
  (let* ([potential-captors
         (filter-map (lambda ([captures : (Listof b:Position)])
                       (opponent-move-captures captures dest))
                     (opponent-captures state src dest))])
    (if (null? potential-captors)
        #f
        potential-captors)))


(: player-lost? (-> turn Boolean))
(define (player-lost? state)
  (not
   (actions-available? (valid-player-turns state))))

(: set-first-turn-fields (-> turn r:Player turn))
(define (set-first-turn-fields state current-player)
  (if (turn-first? state)
      (struct-copy turn state
                   [first-player current-player]
                   [first? #f])
      state))


(: player-flip-location (-> turn b:Position turn))
(define (player-flip-location state coords)
  (let* ([current-player (if (turn-first? state)
                             (player-at-location coords
                                                 (turn-board state))
                             (turn-player state))]
         [next-player (r:toggle-player current-player)]
         [state-started-game (set-first-turn-fields state current-player)])
    (struct-copy turn state-started-game
                 [board (flip-coordinates coords
                                          (turn-board state-started-game))]
                 [player next-player]
                 [message (role-at-location coords
                                            (turn-board state-started-game))]
                 [src-coords b:none-position])))


(: player-move (-> turn b:Position turn))
(define (player-move state dest-coords)
  (let* ([piece-at-dest (piece-at-coordinates dest-coords (turn-board state))]
         [move-check (is-valid-move? state
                                     dest-coords)]
         [response (struct-copy turn state
                                [valid? (car move-check) ]
                                [message (cdr move-check)])])
    (cond
      ((turn-valid? response)
       (struct-copy turn response
                    [player (r:toggle-player (turn-player response))]
                    [board (move-piece-clobber (cast (turn-src-coords response)
                                                     b:Position)
                                               dest-coords
                                               (turn-board response))]
                    [captured piece-at-dest]
                    [src-coords b:none-position]))
      (else
       (struct-copy turn response
                    [captured r:none-role])))))
