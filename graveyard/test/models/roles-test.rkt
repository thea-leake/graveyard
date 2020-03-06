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
         (prefix-in r: "../../models/roles.rkt")
         )

(define player1 (car r:players))
(define player2 (cdr r:players))

(define-test-suite role-hierarchy-value-tests
  "Test hierarchy returns hierarchy value in ascending order from most powerful role"
  (test-case "Test highest in hierarchy returns 0"
    (check-eq? (r:hierarchy-value r:leader)
               0))
  (test-case "Test lowest in heirarchy returns 6 (6 roles used in game)"
    (check-eq? (r:hierarchy-value r:pawn)
               6)))

(run-tests role-hierarchy-value-tests)


(define-test-suite player-roles-tests
  "Test player role list generation"
  (let* ([role-list (r:player-roles player1)]

         [piece-count (length role-list)]

         [player-piece-count (length
                              (filter (lambda (x)
                                        (eq? player1
                                             (r:cell-player x)))
                                      role-list))]

         [piece-revealed-count (length
                                (filter r:cell-revealed?
                                        role-list))]

         [empty-cell-count (length (filter r:cell-empty?
                                           role-list))]
         [leader-count (length
                        (filter (lambda (x)
                                  (eq? r:leader (r:cell-role x)))
                                role-list))]

         [cannon-count (length (filter (lambda (x)
                                         (eq? r:cannon (r:cell-role x)))
                                       role-list))])

    (test-case "16 pieces are generated"
      (check-eq? piece-count
                16))
    (test-case "All 16 pieces belong to the player"
      (check-eq? player-piece-count
                16))
    (test-case "No pieces are revealed"
      (check-eq? piece-revealed-count
                0))
    (test-case "No pieces are empty/none"
      (check-eq? empty-cell-count
                0))
    (test-case "1 leader piece is generated for a player"
      (check-eq? leader-count
                1))
    (test-case "2 cannons are generated for a player"
      (check-eq? cannon-count
                2))
    (test-case "identical pieces use the same id"
      (let*([is-pawn? (lambda (x) (eq? r:pawn (r:cell-role x)))]
            [first-pawn (findf is-pawn? role-list)]
            [pawns-same-id (length
                            (filter (lambda (x)
                                      (eq? first-pawn x))
                                    role-list))])
        (check-eq? pawns-same-id
                   5)))))

(run-tests player-roles-tests)


(define-test-suite toggle-player-tests
  "Test toggle player"
  (test-case "toggles from player1 to player2"
    (check-eq? (r:toggle-player player1)
               player2))
  (test-case "toggles from player2 to player1"
    (check-eq? (r:toggle-player player2)
               player1)))

(run-tests toggle-player-tests)


(define-test-suite flip-piece-tests
  "Test flip - switch cell to revealed"
  (let ([hidden-piece
         (r:cell player1
                 #f ;; revealed
                 r:elephant
                 #f)]

        [revealed-piece
         (r:cell player1
                 #t ;; revealed
                 r:elephant
                 #f)])
    (test-case "piece is switched from hidden to revealed"
      (check-equal? (r:flip hidden-piece)
                   revealed-piece))))

(run-tests flip-piece-tests)
