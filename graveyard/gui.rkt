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
                  send)
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
         (prefix-in g: "graveyard.rkt"))

(define dark-purple-taup
  (make-color 75 65 79))

(define button-width 130)

(define button-height button-width)

(define button-background
  (above (rectangle button-width
                    (* button-height 0.75)
                    "solid"
                    "Blue")
         (rectangle button-width
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

(define button-event (make-channel))

(define game-window (new frame% [label "Graveyard"]))

(define game-pane
  (new pane%
       [parent game-window]))

;; (define game-canvas
;;   (new canvas%
;;        [parent game-pane]))
;; (send game-canvas set-canvas-background dark-purple-taup)

(define vert-arranger
  (new vertical-pane%
       [parent game-pane]))


(define board-container vert-arranger) ;; we'll likely be putting this into a canvas etc.. making it easier to change later

(define welcome-message
  (new message%
       [parent board-container]
       [label welcome-bitmap]))

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


(define (base-revealed-label piece)
  (text (g:role-name piece)
        25
        (g:player-name piece)))

(define (revealed-label piece)
    (add-button-background
     (base-revealed-label piece)))


(define (selected-label piece)
  (add-button-background
   (above (base-revealed-label piece)
          selected-image)))

(define (get-button-label state piece coords)
  (cond
    ((g:piece-empty? piece) empty-plot-label)
    ((and (equal? coords (g:turn-src-coords state))
          (g:piece-revealed? piece))
     (selected-label piece))
    ((g:piece-revealed? piece) (revealed-label piece))
    (else hidden-button-label)))

(define (make-button piece coords)
  (new button%
       [parent board-table]
       [label (get-button-label init-turn piece coords)]
       [callback (lambda (button event)
                   (channel-put button-event coords))]))

(define (update-button state button-piece)
  (send (car button-piece)
        set-label (get-button-label state
                                    (cadr button-piece)
                                    (caddr button-piece))))

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

(define (event-loop init-state)
  (let loop ([state init-state]
             [continue? #t])
    (cond
      (continue? (let* ([click-coords (channel-get button-event)]
                        [event-result (handle-button-click state click-coords)]
                        [next-player-lost? (g:player-lost? event-result)]) ;; checking to see if next player lost based off event handling
                   (loop event-result
                         (not next-player-lost?))))
                 (else (player-won state))))
    (exit))


;; (send game-pane add-child game-canvas)
(send game-window show #t)

(thread
 (lambda () ( event-loop init-turn )))
