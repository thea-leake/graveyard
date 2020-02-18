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


(define-test-suite test-coords-selected?
  "Test Whether coords have been selected by a player"
  (let* ([test-board
          (list
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role   player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]
         [selected-coords (b:get-coords-from-index 10)]
         [selected-turn
          (g:turn test-board
                  player1
                  ""
                  #f
                  r:none-role
                  selected-coords
                  #t)])
    (test-case "A location is selected"
     (check-true (g:coords-selected? selected-turn)))))

(run-tests test-coords-selected?)

(define-test-suite player-move-tests
  "Test player-move moves piece when a move is valid"
  (let* ([move-to-none-role
          (list
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role   player1-zombie r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]
         [moved-to-none-role
          (list
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role  player1-zombie  r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role
           r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role r:none-role)]

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
