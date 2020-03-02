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

(provide board-coordinates
         board-columns
         board-rows
         board-indexes
         get-index-from-coordinates
         get-coords-from-index
         coords-out-of-range?
         coords-row-columns
         gen-board
         Position
         (struct-out position)
         none-position)


(require (only-in racket/list
                  range
                  flatten
                  make-list
                  shuffle)
         "private/definitions.rkt"
         (prefix-in r: "../roles/roles.rkt"))

(require/typed "private/memoized.rkt"
  [board-coordinates (Listof Position)]
  [get-index-from-coordinates (-> Position Index)]
  [get-coords-from-index (-> Index Position)]
  [coords-out-of-range? (-> Position Boolean)]
  [coords-row-columns (-> Position (Listof Position))])

(require/typed racket/list
  [shuffle (-> (Listof r:cell)
               (Listof r:cell))])


(define (gen-board)
  (shuffle (append (r:player-roles (car r:players))
                   (r:player-roles (cdr r:players)))))
