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

#lang racket/base

(require (only-in racket/class
                  new
                  send)
         (only-in racket/gui/base
                  dialog%
                  button%)
         (prefix-in v: "view.rkt")
         (prefix-in ctrl: "../controllers/controller.rkt")
         (prefix-in m: "single-player-start-view.rkt"))


(define start-game-dialog
  (new dialog%
       [label "Choose single or multiplayer"]
       [parent #f]
       [style '(close-button)]
       [enabled #t]
       [width 400]
       [height 100]))


(define single-player-button
  (new button%
       [parent start-game-dialog]
       [label "Single Player"]
       [callback (lambda (button event)
                   (send m:ai-difficulty-dialog show #t)
                   (send start-game-dialog show #f))]))

(define multi-player-button
  (new button%
       [parent start-game-dialog]
       [label "Multi Player"]
       [callback (lambda (button event)
                   (send start-game-dialog show #f)
                   (send v:game-window show #t)
                   (ctrl:local-multi-player))]))

(send start-game-dialog show #t)
