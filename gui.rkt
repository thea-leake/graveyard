#lang racket/gui
(require racket/gui/base)
(require "banqi.rkt")

(define game-window (new frame% [label "Graveyard"]))
(define start-game-msg (new message%
                            [parent game-window]
                            [label "Welcome to Queen of the Graveyard!"]
                            ))
(define row-container game-window) ;; we'll likely be putting this into a canvas etc.. making it easier to change later
(new panel%
     [parent row-container]
     [alignment '(center top)])

(new panel%
     [parent row-container]
     [alignment '(center center)])

(new panel%
     [parent row-container]
     [alignment '(center center)])

(new panel%
     [parent row-container]
     [alignment '(center bottom)])
