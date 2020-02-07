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

(provide end-game-dialog
         end-game-message)

(require (only-in racket/class
                  new
                  send)
         (only-in racket/gui/base
                  message%
                  dialog%
                  button%)
         (prefix-in v: "view.rkt"))

(define end-game-dialog
  (new dialog%
       [label "Game Over!"]
       [parent #f]
       [style '(close-button)]
       [enabled #t]
       [width 200]
       [height 50]))

(define end-game-message
  (new message%
       [parent end-game-dialog]
       [label "Player ??????? has won!"]))

(define confirm-end-game-button
  (new button%
       [parent end-game-dialog]
       [label "OK"]
       [callback (lambda (button event)
                   (send end-game-dialog show #f)
                   (send v:game-window show #f))]))
