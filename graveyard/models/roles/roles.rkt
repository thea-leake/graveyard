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

(require "private/definitions.rkt")

(require/typed
    "private/memoized.rkt"
  [hierarchy-value (-> Role Integer)]
  [player-roles (-> Player (Listof cell))]
  [toggle-player (-> Player Player)]
  [flip (-> cell cell)])

