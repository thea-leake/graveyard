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

(provide ui-updater
         finish-game)

(require (only-in racket/string
                  string-join)
         (only-in racket/class
                  send)
         (prefix-in b: "../models/board.rkt")
         (prefix-in g: "../models/graveyard.rkt")
         (prefix-in t: "../views/tile.rkt")
         (prefix-in v: "../views/view.rkt")
         (prefix-in ev: "../views/end_view.rkt"))

(define (board-tiles gui-player-channel)
  (map (lambda (coords)
         (t:make-tile v:board-table
                      (lambda ()
                        (channel-put gui-player-channel coords))
                      coords))
       b:board-coordinates))


(define (board-updater gui-player-channel)
  (let ([tile-list (board-tiles gui-player-channel)])
    (lambda (state)
     (for-each (lambda (tile-piece-coords)
                 (t:update-tile state tile-piece-coords))
               (map t:location
                    tile-list
                    (g:turn-board state)
                    b:board-coordinates)))))

(define (ui-updater gui-player-channel)
  (let ([update-board (board-updater gui-player-channel)])
    (lambda (state)
      (update-board state)
      (send v:player-display set-label (string-join (list "Current Necromancer:" (g:turn-player state))))
      (send v:player-message set-label (g:turn-message state))
      state)))

(define (finish-game player cleanup-thunk)
  (send ev:end-game-message set-label
        (string-join (list "Player"
                           player
                           "Has Won!")))
  (send ev:end-game-dialog show #t)
  (cleanup-thunk))
