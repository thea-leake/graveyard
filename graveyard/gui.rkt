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

(require (only-in racket/string
                  string-join)
         (only-in racket/class
                  new
                  send)
         (only-in racket/gui/base
                  frame%
                  message%
                  dialog%
                  button%
                  canvas%
                  pane%
                  vertical-pane%)
         (only-in table-panel
                  table-panel%)
         (prefix-in g: "graveyard.rkt")
         (prefix-in ai: "ai.rkt")
         (prefix-in t: "tile.rkt")
         (prefix-in c: "colors.rkt")
         (prefix-in i: "images.rkt"))

(define init-turn
  (g:gen-init-turn "First Necromancer: pick a corpse to raise!"))



(define human-player-channel (make-channel))
(define computer-player-channel (make-channel))

(define game-window (new frame%
                         [label "Graveyard"]))

(define game-pane
  (new pane%
       [parent game-window]))

(define start-game-dialog
  (new dialog%
       [label "Choose single or multiplayer"]
       [parent #f]
       [style '(close-button)]
       [enabled #f]
       [width 400]
       [height 100]))


(define single-player-button
  (new button%
       [parent start-game-dialog]
       [label "Single Player"]
       [callback (lambda (button event)
                   (send start-game-dialog show #f)
                   (send game-window show #t)
                   (single-player))]))


(define multi-player-button
  (new button%
       [parent start-game-dialog]
       [label "Multi Player"]
       [callback (lambda (button event)
                   (send start-game-dialog show #f)
                   (send game-window show #t)
                   (multi-player))]))


(define vert-arranger
  (new vertical-pane%
       [parent game-pane]))


(define board-container vert-arranger)


(define welcome-message
  (new canvas%
       [parent board-container]
       [min-height 50]
       [paint-callback (lambda (me dc)
                         (send dc
                               draw-bitmap
                               i:welcome-bitmap
                               (- (quotient (send me get-width ) 2)
                                  (quotient (send i:welcome-bitmap get-width) 2))
                               0))]))

(send welcome-message set-canvas-background c:dark-purple-taup)

(define player-display-table
  (new table-panel%
       [parent board-container]
       [dimensions '(1 2)]
       [column-stretchability #t]
       [row-stretchability #t]
       [alignment (list 'center 'top)]))


(define player-display
  (new message%
       [parent player-display-table]
       [label (string-join (list
                            "Current Necromancer:" (g:turn-player init-turn)))]))


(define player-message
  (new message%
       [parent player-display-table]
       [label (g:turn-message init-turn)]))


(define board-table
  (new table-panel%
       [parent board-container]
       [border 2]
       [dimensions (list g:board-rows g:board-columns)]
       [alignment (list 'center 'bottom)]))


(define end-game-dialog
  (new dialog%
       [label "Game Over!"]
       [parent #f]
       [style '(close-button)]
       [enabled #f]
       [width 200]
       [height 50]))


(define confirm-end-game-button
  (new button%
       [parent end-game-dialog]
       [label "OK"]
       [callback (lambda (button event)
                   (send end-game-dialog show #f))]))

(define tile-list
  (map (lambda (piece coords)
         (t:make-tile board-table
                      (lambda ()
                        (channel-put human-player-channel coords))
                      piece
                      coords))
       (g:turn-board init-turn)
       g:board-coordinates))


(define (update-board state)
  (for-each (lambda (tile-piece-coords)
              (t:update-tile state tile-piece-coords) )
            (map t:location
                 tile-list
                 (g:turn-board state)
                 g:board-coordinates)))

(define (update-ui state)
  (update-board state)
  (send player-display set-label (string-join (list "Current Necromancer:" (g:turn-player state))))
  (send player-message set-label (g:turn-message state)))


(define (event-handled state)
  (update-ui state)
  state)

(define (finish-move-message state location-coords)
  (let ([captured-piece (g:turn-captured state)])
    (if (g:piece-empty? captured-piece)
        (g:turn-message state)
        (string-join (list "Captured "
                           (g:player-name captured-piece)
                           (g:role-name captured-piece))))))

(define (finish-move-turn state location-coords)
  (let* ([updated-game (g:player-move state
                                      location-coords)]
         [message (finish-move-message updated-game
                                       location-coords)])
    (event-handled (struct-copy g:turn updated-game
                                [message message]
                                [src-coords #f]))))

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
    ((g:turn-src-coords state) (finish-move-turn state location-coords))
    ((g:location-hidden? location-coords (g:turn-board state))
     (raise-location state location-coords))
    ((eq? (g:turn-player state)
          (g:player-at-location location-coords (g:turn-board state)))
     (move-src-event state location-coords))
    (else (wrong-player state))))


(define (player-won state)
  (send end-game-dialog set-label
        (string-join (list "Necromancer "
                           (g:toggle-player (g:turn-player state))
                           "Won!")))
  (send end-game-dialog show #t))

(define (multi-player-event-loop init-state)
  (let loop ([state init-state]
             [continue? #t])
    (cond
      (continue? (let* ([click-coords (channel-get human-player-channel)]
                        [event-result (handle-tile-click state click-coords)]
                        [next-player-lost? (g:player-lost? event-result)]) ;; checking to see if next player lost based off event handling
                   (loop event-result
                         (not next-player-lost?))))
                 (else (player-won state))))
    (exit))


(define (get-input-chnl human-player state)
  (if (eq? (g:turn-player state) human-player)
      human-player-channel
      computer-player-channel))

(define (toggle-input-chnl chnl)
  (if (eq? chnl human-player-channel)
      computer-player-channel
      human-player-channel))


(define (clear-event-chnl chnl)
  (thread
   (lambda ()
     (let loop ([events #t])
      (unless (eq? events 'turn-start)
        (loop (channel-get chnl)))))))

(define (single-player-event-loop init-state)
  (let* ([first-turn (handle-tile-click init-state (channel-get human-player-channel))]
         [human-player (g:toggle-player (g:turn-player first-turn))]
         [human-player? (lambda (state)
                          (eq? (g:turn-player state) human-player))]
         [player-channel (lambda (state)
                           (get-input-chnl human-player state))])
    (let loop ([state first-turn])
      (let ([chnl (player-channel state)])
        (unless (human-player? state)
          (channel-put computer-player-channel state)
          (unless (g:turn-src-coords state)
            (clear-event-chnl human-player-channel)))
        (let* (
               [input-coords (channel-get chnl)]
               [event-result (handle-tile-click state input-coords)]
               [next-player-lost? (g:player-lost? event-result)])
          (unless (or (human-player? state)                  ;; when computer player finished
                      (g:turn-src-coords event-result))      ;; is computer player finished?
            (channel-put human-player-channel 'turn-start))
          (cond
            (next-player-lost? (player-won state))
            (else (loop event-result)))))))
  (exit))


(define (multi-player)
 (thread
  (lambda ()
    (multi-player-event-loop init-turn))))

(define (single-player)
  (thread
   (lambda ()
     (ai:start-ai computer-player-channel)
     (single-player-event-loop init-turn))))

(send start-game-dialog show #t)
