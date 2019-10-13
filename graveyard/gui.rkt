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

(require (only-in pict
                  pict->bitmap)
         (only-in racket/format
                  ~a)
         (only-in racket/string
                  string-join)
         (only-in racket/class
                  new
                  send
                  class
                  define/override
                  define/public
                  super-new
                  object?
                  init-field
                  this
                  inherit)
         (only-in racket/gui/base
                  frame%
                  message%
                  dialog%
                  button%
                  canvas%
                  pane%
                  vertical-pane%)
         (only-in 2htdp/image
                  above
                  text
                  overlay
                  rectangle)
         (only-in racket/draw
                  make-color)
         (only-in table-panel
                  table-panel%)
         (only-in memoize
                  define/memo)
         (prefix-in g: "graveyard.rkt")
         (prefix-in ai: "ai.rkt"))

(define dark-purple-taup
  (make-color 75 65 79))

(define dark-green
  (make-color 0 100 0))

(define button-width 150)

(define button-height button-width)

(define button-background
  (above (rectangle (+ 25 button-width )
                    (* button-height 0.75)
                    "solid"
                    "Blue")
         (rectangle (+ 25 button-width )
                    (* button-height 0.25)
                    "solid"
                    "MediumForestGreen")))

(define coffin-color "LightSlateGray")

(define hidden-button-text
  (above (text (string-join  (list "   --------------"
                                   "/  Still buried   \\"
                                   "|Click to raise!|"
                                   "|     @>-`-,-     |"
                                   )
                             "\n")
               15
               "LightSlateGray")
         (text "| ####-#### |"
               16
               "LightSlateGray")
         (text (make-string 12 #\")
               25
               'MediumForestGreen)))

(define hidden-button-label
  (pict->bitmap
   (overlay hidden-button-text
            button-background)))

(define empty-plot-label
  (let ([rubble (text "%&%*%&@&*%$@%"
                      12
                      'brown)])
    (pict->bitmap
     (overlay (above (text "\n\n\n\nAn Empty Plot!\n"
                           15
                           'black)
                     rubble)
              button-background))))

(define selected-image
  (text (string-join (list "      ----%----  "
                           "[xx|=selected=>")
                     "\n")
        12
        'Goldenrod))

(define welcome-bitmap
  (pict->bitmap (text "Welcome to Queen of the Graveyard!"
                      27
                      "Goldenrod")))


(define init-turn
  (g:gen-init-turn "First Necromancer: pick a corpse to raise!"))

(define human-player-channel (make-channel))
(define computer-player-channel (make-channel))

(define game-window (new frame%
                         [label "Graveyard"]))

(define game-pane
  (new pane%
       [parent game-window]))



(define button-canvas%
  (class canvas%
    (inherit min-width min-height)
    (super-new)
    (init-field callback
                [style (list 'no-autoclear)]
                [prev-image  hidden-button-label]
     )
    (define/public (store-image btmp)
      (set! prev-image btmp))
    (define/public (get-image)
      prev-image)
    (define (my-dc)
      (send this get-dc))
    (define/override (on-event e)
      (when (and (object? e) (send e button-down? 'left))
        (callback)))))

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
                               welcome-bitmap
                               (- (quotient (send me get-width ) 2)
                                  (quotient (send welcome-bitmap get-width) 2))
                               0))]))
(send welcome-message set-canvas-background dark-purple-taup)

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

(define (add-button-background image)
  (pict->bitmap
   (overlay image
            button-background)))


(define/memo (base-revealed-label piece)
  (text (g:role-name piece)
        25
        (g:player-name piece)))

(define/memo (revealed-label piece)
    (add-button-background
     (base-revealed-label piece)))


(define/memo (selected-label piece)
  (add-button-background
   (above (base-revealed-label piece)
          selected-image)))

(define/memo (get-button-label state piece coords)
  (cond
    ((g:piece-empty? piece) empty-plot-label)
    ((and (equal? coords (g:turn-src-coords state))
          (g:piece-revealed? piece))
     (selected-label piece))
    ((g:piece-revealed? piece) (revealed-label piece))
    (else hidden-button-label)))


(define (make-button piece coords)
  (let ([new-button (new button-canvas%
                         [parent board-table]
                         [callback (lambda ()
                                     (channel-put human-player-channel coords))]
                         [min-width button-width]
                         [min-height button-height]
                         [paint-callback (lambda (me dc)
                                               (send dc
                                                     draw-bitmap
                                                     (send me get-image)
                                                     0
                                                     0))])])
    (send new-button set-canvas-background dark-purple-taup)
    (send new-button on-paint)
    new-button))


(define (update-button state button-piece)
  (let ([button-img (get-button-label state
                                      (cadr button-piece)
                                      (caddr button-piece))])
    (send (car button-piece) store-image button-img)
    (send (car button-piece) on-paint)))

(define button-list
  (map make-button
       (g:turn-board init-turn)
       g:board-coordinates))

(define (update-board state)
  (for-each (lambda (button-piece)
              (update-button state button-piece) )
            (map list
                 button-list
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

(define (handle-button-click state location-coords)
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
                        [event-result (handle-button-click state click-coords)]
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
  (let loop ([events? #t])
    (when events?
      (loop (channel-try-get chnl)))))

(define (single-player-event-loop init-state)
  (let* ([first-turn (handle-button-click init-state (channel-get human-player-channel))]
         [human-player (g:toggle-player (g:turn-player first-turn))]
         [human-player? (lambda (state)
                          (eq? (g:turn-player state) human-player))]
         [player-channel (lambda (state)
                           (get-input-chnl human-player state))])
    (ai:start-ai computer-player-channel)
    (let loop ([state first-turn])
      (when (not (human-player? state))
        (channel-put computer-player-channel state))
      (let* ([chnl (player-channel state)]
             [input-coords (channel-get chnl)]
             [event-result (handle-button-click state input-coords)]
             [next-player-lost? (g:player-lost? event-result)])
        (clear-event-chnl chnl)
        (cond
          (next-player-lost? (player-won state))
          (else (loop event-result))))))
  (exit))

;; (send game-pane add-child game-canvas)


(define (multi-player)
 (thread
  (lambda ()
    (multi-player-event-loop init-turn))))

(define (single-player)
  (thread
   (lambda ()
     (single-player-event-loop init-turn))))



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

(send start-game-dialog show #t)
