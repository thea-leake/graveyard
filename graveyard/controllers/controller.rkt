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


(provide single-player
         multi-player)

(require (only-in racket/string
                  string-join)
         (only-in racket/class
                  send)
         (prefix-in b: "../models/board/board.rkt")
         (prefix-in r: "../models/roles/roles.rkt")
         (prefix-in g: "../models/graveyard.rkt")
         (prefix-in ai: "ai.rkt")
         (prefix-in t: "../views/tile.rkt")
         (prefix-in i: "../views/images.rkt")
         (prefix-in v: "../views/view.rkt")
         (prefix-in ev: "../views/end_view.rkt"))

(define init-turn
  (g:gen-init-turn "First Necromancer: pick a corpse to raise!"))

(define human-player-channel (make-channel))

(define computer-player-channel (make-channel))


(define tile-list
  (map (lambda (piece coords)
         (t:make-tile v:board-table
                      (lambda ()
                        (channel-put human-player-channel coords))
                      piece
                      coords))
       (g:turn-board init-turn)
       b:board-coordinates))


(define (update-board state)
  (for-each (lambda (tile-piece-coords)
              (t:update-tile state tile-piece-coords))
            (map t:location
                 tile-list
                 (g:turn-board state)
                 b:board-coordinates)))

(define (update-ui state)
  (update-board state)
  (send v:player-display set-label (string-join (list "Current Necromancer:" (g:turn-player state))))
  (send v:player-message set-label (g:turn-message state)))


(define (event-handled state)
  (update-ui state)
  state)


(define (finish-move-message state location-coords)
  (let ([captured-piece (g:turn-captured state)])
    (if (r:cell-empty? captured-piece)
        (g:turn-message state)
        (string-join (list "Captured "
                           (r:cell-player captured-piece)
                           (r:cell-role captured-piece))))))


(define (finish-move-turn state location-coords)
  (let* ([updated-game (g:player-move state
                                      location-coords)]
         [message (finish-move-message updated-game
                                       location-coords)])
    (event-handled (struct-copy g:turn updated-game
                                [message message]
                                [src-coords b:none-position]))))


(define (raise-message state coords)
  (string-join (list
                "Raised a"
                (g:role-at-location coords (g:turn-board state)))))


(define (raise-location state location-coords)
  (let ([handled-turn (g:player-flip-location state
                                              location-coords)])
    (event-handled
     (struct-copy g:turn handled-turn
                  [message (raise-message state
                                          location-coords )]))))


(define (move-message state location-coords)
  (string-join (list
                (g:player-at-location location-coords (g:turn-board state))
                (g:role-at-location location-coords (g:turn-board state))
                "selected, choose destination")))


(define (move-src-event state location-coords)
  (event-handled (struct-copy g:turn state
                              [src-coords location-coords]
                              [message (move-message state location-coords)])))


(define (wrong-player state)
  (event-handled (struct-copy g:turn state
                              [message "Selected other necromancers piece."])))


(define (handle-tile-click state location-coords)
  (cond
    ((g:coords-selected? state) (finish-move-turn state location-coords))
    ((g:location-hidden? location-coords (g:turn-board state))
     (raise-location state location-coords))
    ((eq? (g:turn-player state)
          (g:player-at-location location-coords (g:turn-board state)))
     (move-src-event state location-coords))
    (else (wrong-player state))))


(define (player-won player cleanup-thunk)
  (send ev:end-game-message set-label
        (string-join (list "Player"
                            player
                           "Has Won!")))
  (send ev:end-game-dialog show #t)
  (cleanup-thunk))


(define (clear-player-channel)
  (when (channel-try-get human-player-channel)
    (clear-player-channel)))

(define (get-human-choice)
  (clear-player-channel)
  (channel-get human-player-channel))


(define (get-computer-choice state)
  (channel-put computer-player-channel state)
  (channel-get computer-player-channel))


(define (event-loop init-state player-choice-fn)
  (let loop ([state init-state])
    (cond
      ((g:player-lost? state)
       (r:toggle-player (g:turn-player state)))         ;; toggle to return winning player
      (else
       (loop
        (handle-tile-click state
                           (player-choice-fn state)))))))


(define (single-player-init-turn init-state)
  (let* ([second-turn
         (handle-tile-click init-state
                            (channel-get human-player-channel))]
         [second-player (g:turn-player second-turn)])
    (event-loop second-turn
                (lambda (state)
                  (if (eq? (g:turn-player state ) second-player)
                      (get-computer-choice state)
                      (get-human-choice))))))


(define (multi-player-init-turn init-state)
  (let ([event-result
         (handle-tile-click init-state
                            (channel-get human-player-channel))])
    (event-loop event-result
                (lambda (_)
                  (get-human-choice)))))


(define (multi-player)
 (thread
  (lambda ()
    (player-won (multi-player-init-turn init-turn)
                (lambda ()
                  (void))))))

(define (single-player [difficulty 'easy])
  (thread
   (lambda ()
     (ai:start-ai computer-player-channel difficulty)
     (player-won (single-player-init-turn init-turn)
                 (lambda ()
                   (channel-put computer-player-channel
                                #f))))))
