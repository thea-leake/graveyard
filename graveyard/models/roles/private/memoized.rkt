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
(provide player-roles
         toggle-player
         flip
         hierarchy-value
         role-hierarchy)
(require (only-in tmemoize
                  memoized
                  memoize)
         "definitions.rkt")

(require/typed racket/list
  [index-of (-> (Listof Role) Role Integer)])


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

