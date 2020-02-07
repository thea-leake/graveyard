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

(provide ai-difficulty-dialog)

(require (only-in racket/class
                  new
                  send)
         (only-in racket/gui/base
                  dialog%
                  button%)
         (prefix-in v: "view.rkt")
         (prefix-in ctrl: "../controller.rkt"))


(define ai-difficulty-dialog
  (new dialog%
       [label "Choose Difficulty Level"]
       [parent #f]
       [style '(close-button)]
       [enabled #t]
       [width 400]
       [height 100]))


(define easy-button
  (new button%
       [parent ai-difficulty-dialog]
       [label "Easy"]
       [callback (lambda (button event)
                   (send ai-difficulty-dialog show #f)
                   (send v:game-window show #t)
                   (ctrl:single-player 'easy))]))


(define medium-button
  (new button%
       [parent ai-difficulty-dialog]
       [label "Medium"]
       [callback (lambda (button event)
                   (send ai-difficulty-dialog show #f)
                   (send v:game-window show #t)
                   (ctrl:single-player 'medium))]))
