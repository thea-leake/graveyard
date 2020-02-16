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



;; Player move tests


(define move-to-empty-location
  (list  r:empty-location  r:empty-location r:empty-location  r:empty-location r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location   player1-zombie  r:empty-location r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location r:empty-location  r:empty-location r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location r:empty-location  r:empty-location r:empty-location   r:empty-location  r:empty-location  r:empty-location))

(define moved-to-empty-location
  (list  r:empty-location  r:empty-location r:empty-location  r:empty-location r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location r:empty-location   player1-zombie  r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location r:empty-location  r:empty-location r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location r:empty-location  r:empty-location r:empty-location   r:empty-location  r:empty-location  r:empty-location))


(define move-selected-coords (b:get-coords-from-index 10)) ;; for memoized obj
(define move-dest-coords (b:get-coords-from-index 11)) ;; for memoized obj

(define before-move-to-empty-location-state
  (g:turn move-to-empty-location
          player1
          ""
          #f
          #f
          move-selected-coords
          #t))

(define after-move-to-empty-location-state
  (g:turn moved-to-empty-location
          player2
          "Valid move."
          #f
          r:empty-location
          #f
          #t))

(define player-move-tests
  (test-suite
   "Test player-move"
   (test-case "Player moving into empty adjacent location"
     (check-equal? (g:player-move before-move-to-empty-location-state
                                 move-dest-coords)
                  after-move-to-empty-location-state))))

(run-tests player-move-tests)

;; Unsafe-move? tests


(define move-with-safe-capture
  (list  r:empty-location  r:empty-location  r:empty-location player1-zombie r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location   player1-zombie  player1-zombie  player2-vampire   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location  r:empty-location player1-zombie r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location r:empty-location r:empty-location r:empty-location  r:empty-location  r:empty-location  r:empty-location))

(define game-state-safe-capture
  (g:turn move-with-safe-capture
        player2
        "" #f #f #f #f))


(define move-with-unsafe-capture
  (list  r:empty-location  r:empty-location  r:empty-location player1-zombie r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location   player1-lich    player1-zombie  player2-vampire   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location  r:empty-location player1-zombie r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location r:empty-location r:empty-location r:empty-location  r:empty-location  r:empty-location  r:empty-location))



(define game-state-unsafe-capture
  (g:turn move-with-unsafe-capture
        player2
        "" #f #f #f #f))

(define move-with-unsafe-capture-by-cannon
  (list  r:empty-location  r:empty-location  r:empty-location player1-zombie r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  player1-wraith   player1-zombie   player1-zombie  player2-vampire   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location  r:empty-location player1-zombie r:empty-location   r:empty-location  r:empty-location  r:empty-location
         r:empty-location  r:empty-location r:empty-location r:empty-location r:empty-location  r:empty-location  r:empty-location  r:empty-location))

(define game-state-unsafe-capture-by-cannon
  (g:turn move-with-unsafe-capture-by-cannon
          player2
          "" #f #f #f #f))

;; this allows us to get the memoized coordinates obj - as we're using eq? for lightweight comparisons
(define src-position
  (b:get-coords-from-index 12)) ;; 4, 1

(define dest-position
  (b:get-coords-from-index 11)) ;; 3, 1

(define opponent-lich-position
  (b:get-coords-from-index 10)) ; 2, 1

(define opponent-cannon-position
  (b:get-coords-from-index 9)) ; 2, 1

(define unsafe-move?-tests
  (test-suite "Tests checking whether moves are safe or not"
              (check-false (g:unsafe-move? game-state-safe-capture
                                          src-position
                                          dest-position))
              (check-equal? (g:unsafe-move? game-state-unsafe-capture
                                          src-position
                                          dest-position)
                            (list opponent-lich-position))
              (check-equal? (g:unsafe-move? game-state-unsafe-capture-by-cannon
                                            src-position
                                            dest-position)
                            (list opponent-cannon-position))))

(run-tests unsafe-move?-tests)
