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


#lang typed/racket/base


(provide Player
         Role
         (struct-out cell)
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
         role-hierarchy
         player-start-roles
         cannon-max-pieces)


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

(require/typed
    racket/list
  [flatten (-> (Listof(Listof String)) (Listof String))]
  [make-list (-> Integer String (Listof Role))])

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
