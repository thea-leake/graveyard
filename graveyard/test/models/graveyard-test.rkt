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

(require rackunit
         rackunit/text-ui
         (only-in racket/list
                  take)
         "../../utils.rkt"
         (prefix-in g: "../../models/graveyard.rkt")
         (prefix-in r: "../../models/roles.rkt")
         (prefix-in b: "../../models/board.rkt")
         )

(define player1 (car r:players))
(define player2 (cdr r:players))

(define player1-zombie
  (r:cell player1
          #t
          r:elephant
          #f))

(define player1-zombie-hidden
  (r:cell player1
          #f
          r:elephant
          #f))

(define player1-lich
  (r:cell player1
          #t
          r:leader
          #f))

(define player1-wraith
  (r:cell player1
          #t
          r:cannon
          #f))

(define player2-vampire
  (r:cell player2
          #t
          r:advisor
          #f))

(define player2-skeleton
  (r:cell player2
          #t
          r:horse
          #f))

(define-test-suite test-location-hidden?
  "Checks whether a location is hidden or revealed"
(let* ([-hidden-role- player1-zombie-hidden]
         [piece-hidden-board
          (list
           r:none-role r:none-role  r:none-role  r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role -hidden-role- r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role  r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role  r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [piece-revealed-board
          (list
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [coords-of-piece (b:get-coords-from-index 10)]

         [empty-coords (b:get-coords-from-index 11)])

    (test-case "a piece at a hidden location"
      (check-true (g:location-hidden? coords-of-piece
                                      piece-hidden-board)))
    (test-case "a piece at a revealed location"
      (check-false (g:location-hidden? coords-of-piece
                                       piece-revealed-board)))
    (test-case "a piece at an empty location"
      (check-false (g:location-hidden? coords-of-piece
                                       piece-revealed-board)))))

(run-tests test-location-hidden?)


(define-test-suite test-role-at-location
  "Gets the role at a location"
  (let* ([board
          (list
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [coords-of-piece (b:get-coords-from-index 10)]

         [empty-coords (b:get-coords-from-index 11)])

    (test-case "Gets the 'Zombie' role of a zombie on the board"
      (check-equal? (g:role-at-location coords-of-piece
                                        board)
                    "Zombie"))

    (test-case "Gets the 'Empty' role of an empty location on the board"
      (check-equal? (g:role-at-location empty-coords
                                        board)
                    "Empty"))))

(run-tests test-role-at-location)

(define-test-suite test-player-at-location
  "Tests getting player of piece at location"
  (let* ([board
          (list
           r:none-role r:none-role   r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie  r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player2-vampire r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role   r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [player1-coords (b:get-coords-from-index 10)]

         [player2-coords (b:get-coords-from-index 18)]

         [empty-coords (b:get-coords-from-index 11)])

    (test-case "Gets player for piece belonging to player1"
      (check-eq? (g:player-at-location player1-coords
                                       board)
                 player1))

    (test-case "Gets player for piece belonging to player2"
      (check-eq? (g:player-at-location player2-coords
                                       board)
                 player2))

    (test-case "Gets 'None' for piece belonging to neither"
      (check-equal? (g:player-at-location empty-coords
                                          board)
                    "None"))))

(run-tests test-player-at-location)

(define-test-suite test-player-flip-location
  "Tests Flipping piece from hidden to visible"
  (let* ([-hidden-role- player1-zombie-hidden]
         [piece-hidden-board
          (list
           r:none-role r:none-role  r:none-role  r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role -hidden-role- r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role  r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role  r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [piece-revealed-board
          (list
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role  r:none-role   r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [coords-to-flip (b:get-coords-from-index 10)]

         [before-flip
          (g:turn piece-hidden-board
                  player1
                  "some message"
                  #f
                  r:none-role
                  b:none-position
                  #t)]

         [after-flip
          (g:turn piece-revealed-board
                  player2
                  "Zombie"
                  #f
                  r:none-role
                  b:none-position
                  #t)])

    (test-case "Flipping a hidden piece reveals that piece and toggles the player"
      (check-equal? (g:player-flip-location before-flip
                                            coords-to-flip)
                    after-flip))))

(run-tests test-player-flip-location)

(define-test-suite test-coords-selected?
  "Test Whether coords have been selected by a player"
  (let* ([test-board
          (list
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [selected-coords (b:get-coords-from-index 10)]

         [selected-turn
          (g:turn test-board
                  player1
                  ""
                  #f
                  r:none-role
                  selected-coords
                  #t)]

         [unselected-turn
          (g:turn test-board
                  player1
                  ""
                  #f
                  r:none-role
                  b:none-position
                  #t)])

    (test-case "A location is selected"
      (check-true (g:coords-selected? selected-turn)))
    (test-case "A location is NOT selected"
      (check-false (g:coords-selected? unselected-turn)))))

(run-tests test-coords-selected?)


(define-test-suite test-gen-init-turn
  "Generates a turn struct with state for a game start"
  (let* ([init-turn (g:gen-init-turn "CAAATS")]

         [compare-board (lambda ()
                          (equal? (g:turn-board init-turn)
                                  (g:gen-init-turn "a message")))])

    (test-case "a different board is generated on different invocations"
      (check-false (and (compare-board)   ;; using and to check again on the chance a duplicate board randomly created
                        (compare-board))))

    (test-case "player set to undecided"
      (check-equal? (g:turn-player init-turn)
                    "Undecided"))

    (test-case "first turn is set to true"
      (check-true (g:turn-first? init-turn)))

    (test-case "init turn has the message passed to gen-init-turn"
      (check-equal? (g:turn-message init-turn)
                    "CAAATS"))

    (test-case "captured piece is set to none-role"
      (check-eq? (g:turn-captured init-turn)
                 r:none-role))

    (test-case "selected position is set to none-position"
      (check-eq? (g:turn-src-coords init-turn)
                 b:none-position))

    (test-case "valid move set to false -- no moves as of yet"
      (check-false (g:turn-valid? init-turn)))))

(run-tests test-gen-init-turn)


(define-test-suite player-move-tests
  "Test player-move moves piece when a move is valid"
  (let* ([move-to-none-role
          (list
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role)]
         [moved-to-none-role
          (list
           r:none-role r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role player1-zombie r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role)]

         [move-selected-coords (b:get-coords-from-index 10)] ;; for memoized obj

         [move-dest-coords (b:get-coords-from-index 11)] ;; for memoized obj

         [before-move-to-none-role-state
          (g:turn move-to-none-role
                  player1
                  ""
                  #f
                  r:none-role
                  move-selected-coords
                  #t)]

         [after-move-to-none-role-state
          (g:turn moved-to-none-role
                  player2
                  "Valid move."
                  #f
                  r:none-role
                  b:none-position
                  #t)]
         [move-to-same-player-occupied-location
          (list
           r:none-role r:none-role r:none-role    r:none-role  r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie player1-lich r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role  r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role  r:none-role r:none-role r:none-role r:none-role)]

         [before-move-to-same-player-occupied-location-state
          (g:turn move-to-same-player-occupied-location
                  player1
                  ""
                  #f
                  r:none-role
                  move-selected-coords
                  #t)]
         [after-move-to-same-player-occupied-location-state
          (g:turn move-to-same-player-occupied-location
                  player1
                  "Cannot capture your own piece"
                  #f
                  r:none-role
                  move-selected-coords
                  #f)])

    (test-case "A piece CAN move to an empty adjacent location"
      (check-equal? (g:player-move before-move-to-none-role-state
                                   move-dest-coords)
                    after-move-to-none-role-state))
    (test-case "A piece CANNOT move into a location already occupied by the same player."
      (check-equal? (g:player-move before-move-to-same-player-occupied-location-state
                                   move-dest-coords)
                    after-move-to-same-player-occupied-location-state))))

(run-tests player-move-tests)


(define-test-suite test-player-lost?
  "Checks to see if a player has lost before start of players turn"
  (let* ([all-player2-pieces-captured
          (list
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [player2-unable-to-move
          (list
           player2-skeleton player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           player1-zombie   player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [player2-flip-available
          (list
           player2-skeleton player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           player1-zombie   player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role player1-zombie-hidden
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [player2-move-available
          (list
           player2-skeleton r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           player1-zombie   player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [all-player2-pieces-captured-state
          (g:turn all-player2-pieces-captured
                  player2
                  "skeleton captured.."
                  #f
                  player2-skeleton
                  b:none-position
                  #t)]

         [player2-unable-to-move-state
          (g:turn player2-unable-to-move
                  player2
                  "skeleton captured.."
                  #f
                  player2-skeleton
                  b:none-position
                  #t)]

         [player2-flip-available-state
          (g:turn player2-flip-available
                  player2
                  "skeleton captured.."
                  #f
                  player2-skeleton
                  b:none-position
                  #t)]

         [player2-move-available-state
          (g:turn player2-move-available
                  player2
                  "skeleton captured.."
                  #f
                  player2-skeleton
                  b:none-position
                  #t)]
         )

    (test-case "when all a players pieces have been captured"
      (check-true (g:player-lost? all-player2-pieces-captured-state)))
    (test-case "when a player has no moves available"
      (check-true (g:player-lost? player2-unable-to-move-state)))
    (test-case "when there is a location available to flip"
      (check-false (g:player-lost? player2-flip-available-state)))
    (test-case "when there is a move available"
      (check-false (g:player-lost? player2-move-available-state)))))

(run-tests test-player-lost?)

(define-test-suite valid-player-turns-tests
  "Test getting valid moves for a player/turn"
  (let* ([all-player2-pieces-captured
          (list
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [player2-unable-to-move
          (list
           player2-skeleton player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           player1-zombie   player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [player2-flip-available
          (list
           player2-skeleton player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           player1-zombie   player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role player1-zombie-hidden
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [player2-move-available
          (list
           player2-skeleton r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           player1-zombie   player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role      r:none-role    r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]

         [all-player2-pieces-captured-state
          (g:turn all-player2-pieces-captured
                  player2
                  "skeleton captured.."
                  #f
                  player2-skeleton
                  b:none-position
                  #t)]

         [player2-unable-to-move-state
          (g:turn player2-unable-to-move
                  player2
                  "skeleton captured.."
                  #f
                  player2-skeleton
                  b:none-position
                  #t)]

         [player2-flip-available-state
          (g:turn player2-flip-available
                  player2
                  "skeleton captured.."
                  #f
                  player2-skeleton
                  b:none-position
                  #t)]

         [player2-move-available-state
          (g:turn player2-move-available
                  player2
                  "skeleton captured.."
                  #f
                  player2-skeleton
                  b:none-position
                  #t)])

    (test-case "when all a players pieces are captured"
      (let* ([available-moves
              (g:valid-player-turns all-player2-pieces-captured-state)]
             [available-captures ((g:actions-captures-thunk available-moves))])
        (check-match available-moves
                     (g:actions #f '() '() _))
        (check-equal? available-captures
                      '())))

    (test-case "when a player is unable to move"
      (let* ([available-moves
              (g:valid-player-turns player2-unable-to-move-state)]
             [available-captures ((g:actions-captures-thunk available-moves))])
        (check-match available-moves
                     (g:actions #f '() '() _))
        (check-equal? available-captures
                      '())))

    (test-case "when a player has moves available"
      (let* ([available-moves
              (g:valid-player-turns player2-move-available-state)]
             [available-captures ((g:actions-captures-thunk available-moves))])
        (check-match available-moves
                     (g:actions
                      #t
                      (list (list (b:position 0 0)
                                  (b:position 1 0)))
                      '()
                      _))
        (check-equal? available-captures
                      '())))

    (test-case "when a player has a flip available"
      (let* ([available-moves
              (g:valid-player-turns player2-flip-available-state)]
             [available-captures ((g:actions-captures-thunk available-moves))])
        (check-match available-moves
                     (g:actions #t
                                '()
                                (list (b:position 7 2))
                                _))
        (check-equal? available-captures
                      '())))))

(run-tests valid-player-turns-tests)

(define-test-suite unsafe-move?-tests
  "Test potential move gets list of pieces that can capture it, or false if none"
  (let* ([move-with-safe-capture
          (list
           r:none-role r:none-role r:none-role    player1-zombie r:none-role     r:none-role r:none-role r:none-role
           r:none-role r:none-role player1-zombie player1-zombie player2-vampire r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    player1-zombie r:none-role     r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role    r:none-role    r:none-role     r:none-role r:none-role r:none-role)]

         [game-state-safe-capture
          (g:turn move-with-safe-capture
                  player2
                  "" #f #f #f #f)]

         [move-with-unsafe-capture
          (list
           r:none-role r:none-role r:none-role  player1-zombie r:none-role     r:none-role     r:none-role r:none-role
           r:none-role r:none-role player1-lich player1-zombie player2-vampire r:none-role     r:none-role r:none-role
           r:none-role r:none-role r:none-role  player1-zombie r:none-role     r:none-role     r:none-role r:none-role
           r:none-role r:none-role r:none-role  r:none-role    r:none-role     r:none-role     r:none-role r:none-role)]

         [game-state-unsafe-capture
          (g:turn move-with-unsafe-capture
                  player2
                  "" #f #f #f #f)]

         [move-with-unsafe-capture-by-cannon
          (list
           r:none-role r:none-role    r:none-role    player1-zombie   r:none-role     r:none-role r:none-role r:none-role
           r:none-role player1-wraith player1-zombie player1-zombie   player2-vampire r:none-role r:none-role r:none-role
           r:none-role r:none-role    r:none-role    player1-zombie   r:none-role     r:none-role r:none-role r:none-role
           r:none-role r:none-role    r:none-role    r:none-role      r:none-role     r:none-role r:none-role r:none-role)]

         [game-state-unsafe-capture-by-cannon
          (g:turn move-with-unsafe-capture-by-cannon
                  player2
                  "" #f #f #f #f)]

         ;; this allows us to get the memoized coordinates obj - as we're using eq? for lightweight comparisons
         [src-position             ;; 4, 1
          (b:get-coords-from-index 12)]

         [dest-position            ;; 3, 1
          (b:get-coords-from-index 11)]

         [opponent-lich-position   ;; 2, 1
          (b:get-coords-from-index 10)]

         [opponent-cannon-position ;; 2, 1
          (b:get-coords-from-index 9)])

    (test-case "A piece can capture an opponent piece and the opponent CANNOT capture it next turn"
      (check-false (g:unsafe-move? game-state-safe-capture
                                  src-position
                                  dest-position)))
    (test-case "A piece can capture an opponent piece and the opponent CAN capture it next turn"
      (check-equal? (g:unsafe-move? game-state-unsafe-capture
                                   src-position
                                   dest-position)
                   (list opponent-lich-position)))
    (test-case "A piece can capture an opponent piece and the opponent cannon CAN capture it next turn"
      (check-equal? (g:unsafe-move? game-state-unsafe-capture-by-cannon
                                   src-position
                                   dest-position)
                   (list opponent-cannon-position)))))

(run-tests unsafe-move?-tests)
