#lang racket/gui
(require racket/gui/base)
(require racket/format)
(require table-panel)

(require (prefix-in b: "banqi.rkt"))

(provide (prefix-out gui: (all-defined-out)))

(define display-panel-min-width 350)
(define display-panel-min-height 100)

(struct turn
  (selected-coords
   board
   player
   message))

(define init-turn
  (turn
   #f          ;; selected-coords
   (b:gen-board) ;; board
   "Undecided" ;; player
   "First player: pick a corpse to raise!" ;; message
   ))

(define first-turn (make-parameter #t))

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
                            "Current Player:" (turn-player init-turn)))]
       [min-width display-panel-min-width]))


(define player-message
  (new message%
       [parent player-display-table]
       [label (turn-message init-turn)]
       [min-width display-panel-min-width]))


(define (location-format state)
  (let ([selected-coords (turn-selected-coords state)])
    (if selected-coords
        (string-join (list "Selected Square:"
                           (~a selected-coords)
                           ","
                           (~a selected-coords)))
        "Nothing Selected")))


(define location-selected
  (new message%
       [parent player-display-table]
       [label (location-format init-turn)]
       [min-width display-panel-min-width]))

(define board-table
  (new table-panel%
       [parent board-container]
       [border 2]
       [dimensions (list b:board-rows b:board-columns)]))


(define end-game-dialog
  (new dialog% [label "Game Over!"]
       [parent #f]
       [style '(close-button)]
       [enabled #f]
       [width 200]
       [height 50]))


(define (risen-label piece)
  (~a (b:player-name piece)
      "\n---\n"
      (b:role-name piece)))

(define (get-button-label state piece coords)
  (cond
    ((b:piece-empty? piece) "An empty Plot!")
    ((and (equal? coords (turn-selected-coords state))
          (b:piece-revealed? piece))
     (string-join (list (risen-label piece)
                        "--%--"
                        "[xx|=selected=>")
                  "\n"))
    ((b:piece-revealed? piece) (risen-label piece))
    (else (~a (string-join  (list "----------"
                                  "/  Still buried  \\"
                                  "|Click to raise!|"
                                  "|    @>-`-,-     |"
                                  "| ####-#### |"
                                  (make-string 18 #\"))
                            "\n")))))

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
       (turn-board init-turn)
       b:board-coordinates))

(define (update-board state)
  (for-each (lambda (button-piece)
              (update-button state button-piece) )
            (map list
                 button-list
                 (turn-board state)
                 b:board-coordinates)))

(define (update-ui state)
  (update-board state)
  (send player-display set-label (string-join (list "Current Player:" (turn-player state))))
  (send player-message set-label (turn-message state))
  (send location-selected set-label (location-format state)))

(define (event-handled state)
  (update-ui state)
  state)

(define (finish-move-message updated-game location-coords)
  (let ([captured-piece (hash-ref updated-game 'captured)])
    (if (b:piece-empty? captured-piece)
        (hash-ref updated-game 'message)
        (string-join (list "Captured "
                           (b:player-name captured-piece)
                           (b:role-name captured-piece))))))

(define (finish-move-turn state location-coords)
  (let* ([updated-game (b:player-move (turn-player state)
                                    (turn-selected-coords state)
                                    location-coords
                                    (turn-board state))]
         [message (finish-move-message updated-game
                                       location-coords)])
    (event-handled (struct-copy turn state
                                [player (hash-ref updated-game 'player)]
                                [message message]
                                [board (hash-ref updated-game 'board)]
                                [selected-coords #f]))))

(define (raise-location state location-coords)
  (let ([player (if (first-turn)
                    (b:player-at-location location-coords (turn-board state))
                    (turn-player state))])
    (parameterize ([first-turn #f])
      (event-handled (struct-copy turn state
                                  [board (b:flip-coordinates location-coords (turn-board state))]
                                  [player (b:toggle-player player)]
                                  [message (string-join (list "Raised a " (b:role-at-location location-coords (turn-board state))))]
                                  [selected-coords #f])))))

(define (move-message state location-coords)
  (string-join (list
                (b:player-at-location location-coords (turn-board state))
                (b:role-at-location location-coords (turn-board state))
                "selected, choose destination")))

(define (move-src-event state location-coords)
  (event-handled (struct-copy turn state
                              [selected-coords location-coords]
                              [message (move-message state location-coords)])))

(define (wrong-player state)
  (event-handled (struct-copy turn state
                              [message "Selected other players piece."])))

(define (handle-button-click state location-coords)
  (cond
    ((turn-selected-coords state) (finish-move-turn state location-coords))
    ((b:location-hidden? location-coords (turn-board state))
     (raise-location state location-coords))
    ((eq? (turn-player state)
          (b:player-at-location location-coords (turn-board state)))
     (move-src-event state location-coords))
    (else (wrong-player state))))


(define (player-won state)
  (send end-game-dialog set-label
        (string-join (list "Player"
                           (b:toggle-player (turn-player state))
                           "Won!")))
  (send end-game-dialog show #t))

(define (event-loop init-state)
  (let loop ([state init-state]
             [continue? #t])
    (cond
      (continue? (let* ([click-coords (channel-get button-event)]
                        [event-result (handle-button-click state click-coords)]
                        [next-player-lost? (b:player-lost? (turn-player event-result)
                                                         (turn-board event-result))]) ;; checking to see if next player lost based off event handling
                   (loop event-result
                         (not next-player-lost?))))
                 (else (player-won state))))
    (exit))

(send game-window show #t)

(thread
 (lambda () ( event-loop init-turn )))
