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

(provide Player
         Role
         role-hierarchy
         leader
         advisor
         elephant
         chariot
         horse
         cannon
         pawn
         empty
         none-role
         players
         player-roles
         toggle-player
         role-hierarchy
         hierarchy-value
         cannon-max-pieces
         flip
         (struct-out cell))


(require tmemoize)
(require/typed
    racket/list
  [flatten (-> (Listof(Listof String)) (Listof String))]
  [make-list (-> Integer String (Listof Role))]
  [index-of (-> (Listof Role) Role Integer)])


(define-type None False)

(define none : None #f)

(define-type Player (U String None))

(define-type Role String)


(struct cell
  ([player : Player]
   [revealed? : Boolean]
   [role : Role]
   [empty? : Boolean])
  #:transparent)


;; roles
(define leader : Role "Lich")
(define advisor : Role "Vampire")
(define elephant : Role "Zombie")
(define chariot : Role "Ghoul")
(define horse : Role "Skeleton")
(define cannon : Role "Wraith")
(define pawn : Role "Poltergeist")
(define empty : Role "Empty")


(: players (Pair Player Player))
(define players
  (cons "Orange" "Purple"))


(define none-role
  (cell none       ;; player
        #t         ;; revealed
        empty      ;; role
        #t))       ;; empty


;; it is worth noting that this is not referenced when the cannon is capturing
;; cannons can capture any unit, and any unit except soldier can capture the cannon
(: role-hierarchy (Listof Role))
(define role-hierarchy
  (list leader advisor elephant chariot horse cannon pawn empty))


;; number of pieces that can be involve in a cannon move
;; cannon, piece cannon jumps over, piece cannon takes
(define cannon-max-pieces : Integer 3)



(: player-start-roles (Listof String))
(define player-start-roles
  (flatten
   (list (make-list 1 leader)
         (make-list 2 advisor)
         (make-list 2 elephant)
         (make-list 2 chariot)
         (make-list 2 horse)
         (make-list 5 pawn)
         (make-list 2 cannon))))

(memoized
 (: hierarchy-value (-> Role Integer))
 (define (hierarchy-value role)
   (index-of role-hierarchy role)))


;; the memoized lambda lets us memoize the the pieces of the same data
;; so they share the same obj id - which lets us memoize more effectively
;; further down the line w/ only obj id checks
;; using a memoized lambda instead of define/memo to allow these object to be cleaned up by GC when no longer used.
(: piece-maker (-> (-> Player Role cell)))
(define (piece-maker)
  (memoize (lambda ([player : Player] [role : Role])
             (cell player     ;; player
                   #f         ;; revealed
                   role       ;;
                   #f))))      ;; empty


(: player-roles (-> Player (Listof cell)))
(define (player-roles team)
  (let ([mkpiece : (-> Player Role cell) (piece-maker)])
    (map (lambda ([role : Role]) (mkpiece team role))
         player-start-roles)))


(memoized
 (: toggle-player (-> Player Player))
 (define (toggle-player player)
   (if (eq? player (car players))
       (cdr players)
       (car players))))


(memoized
 (: flip (-> cell cell))
 (define(flip piece)
   (struct-copy cell piece
                [revealed? #t])))

