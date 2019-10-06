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
                  button%)
         (only-in 2htdp/image
                  above
                  text)
         (only-in table-panel
                  table-panel%)
         (prefix-in b: "banqi.rkt"))


(define display-panel-min-width 350)
(define display-panel-min-height 100)

(define init-turn
  (b:gen-init-turn "First player: pick a corpse to raise!"))


(define button-event (make-channel))

(define game-window (new frame% [label "Graveyard"]))
(define start-game-msg (new message%
                            [parent game-window]
                            [label "Welcome to Queen of the Graveyard!"]))

(define board-container game-window) ;; we'll likely be putting this into a canvas etc.. making it easier to change later

(define player-display-table
  (new table-panel%
       [parent board-container]
       [dimensions '(2 2)]
       [column-stretchability #t]
       [row-stretchability #t]
       [min-height display-panel-min-height]))


(define player-display
  (new message%
       [parent player-display-table]
       [label (string-join (list
                            "Current Player:" (b:turn-player init-turn)))]
       [min-width display-panel-min-width]))


(define player-message
  (new message%
       [parent player-display-table]
       [label (b:turn-message init-turn)]
       [min-width display-panel-min-width]))


(define board-table
  (new table-panel%
       [parent board-container]
       [border 2]
       [dimensions (list b:board-rows b:board-columns)]))


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

(define (risen-label piece)
    (pict->bitmap
     (text (b:role-name piece)
           25
           (b:player-name piece)
            )))

(define hidden-button-label
  (pict->bitmap
   (above (text (string-join  (list "   --------------"
                                    "/  Still buried   \\"
                                    "|Click to raise!|"
                                    "|     @>-`-,-     |"
                                    )
                              "\n")
          15
          'DarkSlateBlue)
          (text "| ####-#### |"
                16
                'DarkSlateBlue)
          (text (make-string 12 #\")
                25
                'darkgreen))))

(define empty-plot-label
  (let ([rubble (text "%&%*%&@&*%"
                      15
                      'brown)])
    (pict->bitmap
     (above rubble
            (text "An Empty Plot!"
                  15
                  'black)
            rubble))))

(define (selected-label piece)
  (pict->bitmap
   (above (risen-label piece)
          (text (string-join (list "      ----%----  "
                                   "[xx|=selected=>")
                             "\n")
                12
                'ForestGreen))))

(define (get-button-label state piece coords)
  (cond
    ((b:piece-empty? piece) empty-plot-label)
    ((and (equal? coords (b:turn-src-coords state))
          (b:piece-revealed? piece))
     (selected-label piece))
    ((b:piece-revealed? piece) (risen-label piece))
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
       (b:turn-board init-turn)
       b:board-coordinates))

(define (update-board state)
  (for-each (lambda (button-piece)
              (update-button state button-piece) )
            (map list
                 button-list
                 (b:turn-board state)
                 b:board-coordinates)))

(define (update-ui state)
  (update-board state)
  (send player-display set-label (string-join (list "Current Player:" (b:turn-player state))))
  (send player-message set-label (b:turn-message state)))

(define (event-handled state)
  (update-ui state)
  state)

(define (finish-move-message state location-coords)
  (let ([captured-piece (b:turn-captured state)])
    (if (b:piece-empty? captured-piece)
        (b:turn-message state)
        (string-join (list "Captured "
                           (b:player-name captured-piece)
                           (b:role-name captured-piece))))))

(define (finish-move-turn state location-coords)
  (let* ([updated-game (b:player-move state
                                      location-coords)]
         [message (finish-move-message updated-game
                                       location-coords)])
    (event-handled (struct-copy b:turn updated-game
                                [message message]
                                [src-coords #f]))))

(define (raise-message state coords)
  (string-join (list
                "Raised a"
                (b:role-at-location coords
                                    (b:turn-board state)))))

(define (raise-location state location-coords)
  (let ([next-player (if (b:turn-first? state)
                         (b:toggle-player (b:player-at-location location-coords
                                               (b:turn-board state)))
                         (b:toggle-player (b:turn-player state)))])
    (event-handled (struct-copy b:turn state
                                [board (b:flip-coordinates location-coords
                                                           (b:turn-board state))]
                                [player next-player]
                                [message (raise-message state
                                                        location-coords)]
                                [src-coords #f]
                                [first? #f]))))

(define (move-message state location-coords)
  (string-join (list
                (b:player-at-location location-coords (b:turn-board state))
                (b:role-at-location location-coords (b:turn-board state))
                "selected, choose destination")))

(define (move-src-event state location-coords)
  (event-handled (struct-copy b:turn state
                              [src-coords location-coords]
                              [message (move-message state location-coords)])))

(define (wrong-player state)
  (event-handled (struct-copy b:turn state
                              [message "Selected other players piece."])))

(define (handle-button-click state location-coords)
  (cond
    ((b:turn-src-coords state) (finish-move-turn state location-coords))
    ((b:location-hidden? location-coords (b:turn-board state))
     (raise-location state location-coords))
    ((eq? (b:turn-player state)
          (b:player-at-location location-coords (b:turn-board state)))
     (move-src-event state location-coords))
    (else (wrong-player state))))


(define (player-won state)
  (send end-game-dialog set-label
        (string-join (list "Player"
                           (b:toggle-player (b:turn-player state))
                           "Won!")))
  (send end-game-dialog show #t))

(define (event-loop init-state)
  (let loop ([state init-state]
             [continue? #t])
    (cond
      (continue? (let* ([click-coords (channel-get button-event)]
                        [event-result (handle-button-click state click-coords)]
                        [next-player-lost? (b:player-lost? event-result)]) ;; checking to see if next player lost based off event handling
                   (loop event-result
                         (not next-player-lost?))))
                 (else (player-won state))))
    (exit))

(send game-window show #t)

(thread
 (lambda () ( event-loop init-turn )))
