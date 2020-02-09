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
                  range
                  flatten
                  make-list
                  shuffle
                  index-of)
         (only-in memoize
                  define/memo
                  memo-lambda)
         (prefix-in u: "../utils.rkt"))

(provide role-hierarchy
         leader
         advisor
         elephant
         chariot
         horse
         cannon
         pawn
         empty-role
         empty-location
         players
         player-roles
         toggle-player
         role-hierarchy
         hierarchy-value
         cannon-max-pieces
         flip
         (struct-out cell))


(struct cell
  (player
   revealed?
   role
   empty?)
  #:transparent)

(define board-rows 4)
(define board-columns 8)
(define location-count (* board-columns
                          board-rows))


(define players
  (cons "Orange" "Purple"))


;; roles
(define leader "Lich")
(define advisor "Vampire")
(define elephant "Zombie")
(define chariot "Ghoul")
(define horse "Skeleton")
(define cannon "Wraith")
(define pawn "Poltergeist")
(define empty-role "Empty")


;; it is worth noting that this is not referenced when the cannon is capturing
;; cannons can capture any unit, and any unit except soldier can capture the cannon
(define role-hierarchy
  (list leader advisor elephant chariot horse cannon pawn empty-role))

(define/memo (hierarchy-value role)
  (index-of role-hierarchy role))


;; number of pieces that can be involve in a cannon move
;; cannon, piece cannon jumps over, piece cannon takes
(define cannon-max-pieces 3)

(define player-start-roles
  (flatten
   (list (make-list 1 leader)
         (make-list 2 advisor)
         (make-list 2 elephant)
         (make-list 2 chariot)
         (make-list 2 horse)
         (make-list 5 pawn)
         (make-list 2 cannon))))


;; the memoized lambda lets us memoize the the pieces of the same data
;; so they share the same obj id - which lets us memoize more effectively
;; further down the line w/ only obj id checks
;; using a memoized lambda instead of define/memo to allow these object to be cleaned up by GC when no longer used.
(define (piece-maker)
  (memo-lambda (player role)
   (cell player     ;; player
         #f         ;; revealed
         role       ;;
         #f)))       ;; empty

(define empty-location
  (cell #f         ;; player
        #t         ;; revealed
        empty-role ;; role
        #t))       ;; empty


(define (player-roles team)
  (let ([mkpiece (piece-maker)])
    (map (lambda (role) (mkpiece team role))
        player-start-roles)))


(define/memo (toggle-player player)
  (if (equal? player (car players))
      (cdr players)
      (car players)))


(define/memo (flip piece)
  (struct-copy cell piece
               [revealed? #t]))
