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

(define player2-vampire
  (r:cell player2
          #t
          r:advisor
          #f))

(define test-known-board
  (list
   (r:cell player1 #f r:cannon #f) (r:cell player1 #f r:horse #f) (r:cell player2 #f r:leader #f) (r:cell player2 #f r:pawn #f) (r:cell player1 #f r:elephant #f) (r:cell player2 #f r:chariot #f) (r:cell player2 #f r:elephant #f) (r:cell player1 #f r:advisor #f)
   (r:cell player2 #f r:cannon #f) (r:cell player1 #f r:horse #f) (r:cell player2 #f r:horse #f) (r:cell player1 #f r:pawn #f) (r:cell player1 #f r:cannon #f) (r:cell player2 #f r:cannon #f) (r:cell player1 #f r:chariot #f) (r:cell player2 #f r:advisor #f)
   (r:cell player1 #f r:pawn #f) (r:cell player2 #f r:pawn #f) (r:cell player2 #f r:pawn #f) (r:cell player2 #f r:pawn #f) (r:cell player1 #f r:advisor #f) (r:cell player1 #f r:leader #f) (r:cell player1 #f r:chariot #f) (r:cell player1 #f r:pawn #f)
   (r:cell player1 #f r:pawn #f) (r:cell player2 #f r:chariot #f) (r:cell player2 #f r:pawn #f) (r:cell player1 #f r:elephant #f) (r:cell player2 #f r:advisor #f) (r:cell player2 #f r:horse #f) (r:cell player1 #f r:pawn #f) (r:cell player2 #f r:elephant #f)))



;; Test gen-board - check tests working
(define test-board (b:gen-board))

(define gen-board-tests
  (test-suite "Test board generation"
              ;; board created w/ 32 r:cells
              (check-equal? (length test-board)
                            32)
              ;; all r:cells start out hidden
              (check-equal? (length (filter (lambda (x)
                                             (r:cell-revealed? x))
                                            test-board))
                            0)
              ;; player count split in half
              (check-equal? (length (filter (lambda (x)
                                              (eq? (r:cell-player x)
                                                   player1))
                                            test-board))
                            16)))

(run-tests gen-board-tests)

;; toggle player
(define toggle-player-tests
  (test-suite "Test toggle player"
              (check-equal? (r:toggle-player player1)
                            player2)
              (check-equal? (r:toggle-player player2)
                            player1)))

(run-tests toggle-player-tests)

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

;; this allows us to get the memoized coordinates obj - as we're using eq? for lightweight comparisons
(define src-position
  (b:get-coords-from-index 12)) ;; 4, 1

(define dest-position
  (b:get-coords-from-index 11)) ;; 3, 1

(define opponent-greater-position
  (b:get-coords-from-index 10)) ; 2, 1

(define unsafe-move?-tests
  (test-suite "Tests checking whether moves are safe or not"
              (check-equal? (g:unsafe-move? game-state-safe-capture
                                          src-position
                                          dest-position)
                            #f)
              (check-equal? (g:unsafe-move? game-state-unsafe-capture
                                          src-position
                                          dest-position)
                            (list opponent-greater-position))))

(run-tests unsafe-move?-tests)
