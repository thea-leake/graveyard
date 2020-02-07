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
         )

(define player1 (car r:players))
(define player2 (cdr r:players))

(define role-hierarchy-value-tests
  (test-suite "Test getting role hierarchy"
              (check-equal? (r:hierarchy-value r:leader)
                            0)
              (check-equal? (r:hierarchy-value r:pawn)
                            6)))

(run-tests role-hierarchy-value-tests)


(define player-roles-tests
  (test-suite "Test player role list generation"
              (check-equal? (length (r:player-roles player1))
                            16)
              (check-equal? (length (filter (lambda (x)
                                              (eq? player1 (r:cell-player x)))
                                            (r:player-roles player1)))
                            16)
              (check-equal? (length (filter r:cell-revealed?
                                            (r:player-roles player1)))
                            0)
              (check-equal? (length (filter r:cell-empty?
                                            (r:player-roles player1)))
                            0)
              (check-equal? (length (filter (lambda (x)
                                              (eq? r:leader (r:cell-role x)))
                                            (r:player-roles player1)))
                            1)
              (check-equal? (length (filter (lambda (x)
                                              (eq? r:cannon (r:cell-role x)))
                                            (r:player-roles player1)))
                            2)
              (check-equal? (length (filter (lambda (x)
                                              (eq? r:pawn (r:cell-role x)))
                                            (r:player-roles player1)))
                            5)))

(run-tests player-roles-tests)


(define toggle-player-tests
  (test-suite "Test toggle player"
              (check-equal? (r:toggle-player player1)
                            player2)
              (check-equal? (r:toggle-player player2)
                            player1)))

(run-tests toggle-player-tests)
