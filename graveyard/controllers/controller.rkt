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


(provide local-single-player
         local-multi-player)

(require (only-in racket/string
                  string-join)
         (prefix-in b: "../models/board.rkt")
         (prefix-in r: "../models/roles.rkt")
         (prefix-in g: "../models/graveyard.rkt")
         (prefix-in ai: "ai.rkt")
         (prefix-in gu: "gui.rkt"))


(define computer-player-channel (make-channel))

(define gui-player-channel (make-channel))


(define (init-turn)
  (g:gen-init-turn "First Necromancer: pick a corpse to raise!"))


(define (finish-move-message state)
  (let ([captured-piece (g:turn-captured state)])
    (if (r:cell-empty? captured-piece)
        (g:turn-message state)
        (string-join (list "Captured "
                           (r:cell-player captured-piece)
                           (r:cell-role captured-piece))))))


(define (finish-move-turn state location-coords)
  (let* ([updated-game (g:player-move state
                                      location-coords)]
         [message (finish-move-message updated-game)])
    (struct-copy g:turn updated-game
                 [message message]
                 [src-coords b:none-position])))


(define (raise-message state coords)
  (string-join (list
                "Raised a"
                (g:role-at-location coords (g:turn-board state)))))


(define (raise-location state location-coords)
  (let ([handled-turn (g:player-flip-location state)])
    (struct-copy g:turn handled-turn
                 [message (raise-message state
                                         location-coords )])))


(define (move-message state location-coords)
  (string-join (list
                (g:player-at-location location-coords (g:turn-board state))
                (g:role-at-location location-coords (g:turn-board state))
                "selected, choose destination")))


(define (move-src-event state location-coords)
  (struct-copy g:turn state
               [src-coords location-coords]
               [message (move-message state location-coords)]))


(define (wrong-player state)
  (struct-copy g:turn state
               [message "Selected other necromancers piece."]))


(define (handle-player-action state location-coords)
  (cond
    ((g:coords-selected? state) (finish-move-turn state location-coords))
    ((g:location-hidden? location-coords (g:turn-board state))
     (raise-location state location-coords))
    ((eq? (g:turn-player state)
          (g:player-at-location location-coords (g:turn-board state)))
     (move-src-event state location-coords))
    (else (wrong-player state))))



(define (clear-player-channel)
  (when (channel-try-get gui-player-channel)
    (clear-player-channel)))

(define (get-human-choice)
  (clear-player-channel)
  (channel-get gui-player-channel))


(define (get-computer-choice state)
  (channel-put computer-player-channel state)
  (channel-get computer-player-channel))


(define (event-loop init-state client-updater player-choice-fn)
  (let loop ([state init-state])
    (cond
      ((g:player-lost? state)
       ;; toggle to return winning player
       (r:toggle-player (g:turn-player state)))
      (else
       (loop
        (client-updater (handle-player-action state
                               (player-choice-fn state))))))))


;; Starts and runs a single player game.
;; Returns when the game is won
(define (single-player-start-game init-state)
  (let ([update-ui (gu:ui-updater gui-player-channel init-state)])
    (event-loop init-state
                update-ui
                (lambda (state)
                 (if (eq? (g:turn-player state )
                          (g:turn-first-player state))
                     ;; as of now ai goes second
                     (get-human-choice)
                     (get-computer-choice state))))))


;; Starts and runs a multi player game
;; Finishes when the game is won
(define (multi-player-start-game init-state)
  (let ([update-ui (gu:ui-updater gui-player-channel init-state)])
    (event-loop init-state
                update-ui
                (lambda (_)
                  (get-human-choice)))))


(define (local-multi-player)
  (thread
   (lambda ()
     (gu:finish-game (multi-player-start-game (init-turn))
                  void))))

(define (local-single-player [difficulty 'easy])
  (thread
   (lambda ()
     (ai:start-ai computer-player-channel difficulty)
     (gu:finish-game (single-player-start-game (init-turn))
                  (lambda ()
                    (channel-put computer-player-channel
                                 #f))))))
