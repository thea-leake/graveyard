#lang racket/gui
(require racket/gui/base)
(require racket/format)
(require table-panel)
;; using: gen-board board-coordinates role-name board-rows board-columns piece-revealed? piece-empty? player-move 
;; location-hidden? flip-coordinates toggle-player role-at-location player-at-location
(require "banqi.rkt")

(provide (all-defined-out))

(define display-panel-min-width 350)
(define display-panel-min-height 100)

(define partial-turn (make-parameter #f))
(define src-coords (make-parameter #f))
(define board (make-parameter (gen-board)))
(define first-turn (make-parameter #t))
(define current-player (make-parameter "Undecided"))
(define current-message (make-parameter "First player: pick a corpse to raise!"))
(define captured-red-pieces (make-parameter '()))
(define captured-black-pieces (make-parameter '()))

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
                            "Current Player:" (current-player)))]
       [min-width display-panel-min-width]))


(define player-message
  (new message%
       [parent player-display-table]
       [label (current-message)]
       [min-width display-panel-min-width]))


(define (location-format)
  (if (src-coords)
      (string-join (list "Selected Square:"
                         (~a (x-pos (src-coords)))
                         ","
                         (~a (y-pos (src-coords)))))
      "Nothing Selected"))

(define location-selected
  (new message%
       [parent player-display-table]
       [label (location-format)]
       [min-width display-panel-min-width]))

(define board-table
  (new table-panel%
       [parent board-container]
       [border 2]
       [dimensions (list board-rows board-columns)]))


(define (risen-label piece)
  (~a (player-name piece)
      "\n---\n"
      (role-name piece)))

(define (get-button-label piece coords)
  (cond
    ((piece-empty? piece) "An empty Plot!")
    ((and (equal? coords (src-coords))
          (piece-revealed? piece))
     (string-join (list (risen-label piece)
                        "--%--"
                        "[xx|=selected=>")
                  "\n"))
    ((piece-revealed? piece) (risen-label piece))
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
       [label (get-button-label piece coords)]
       [callback (lambda (button event)
                   (channel-put button-event coords))]))

(define (update-button button-piece)
  (send (car button-piece)
        set-label (get-button-label (cadr button-piece)
                                    (caddr button-piece))))

(define button-list
  (map make-button
       (board)
       board-coordinates))

(define (update-board)
  (for-each update-button
            (map list
                 button-list
                 (board)
                 board-coordinates)))

(define (update-ui)
  (update-board)
  (send player-display set-label (string-join (list "Current Player:" (current-player))))
  (send player-message set-label (current-message))
  (send location-selected set-label (location-format)))

(define (finish-move-turn location-coords)
  (let ([updated-game (player-move (current-player)
                                   (src-coords)
                                   location-coords
                                   (board))])
    (parameterize ([current-player (hash-ref updated-game 'player)]
                   [current-message (hash-ref updated-game 'message)]
                   [board (hash-ref updated-game 'board)]
                   [partial-turn #f]
                   [src-coords #f])
      (update-ui)
      (next-event))))

(define (raise-location location-coords)
  (let ([player (if (first-turn)
                    (player-at-location location-coords (board))
                    (current-player))])
    (parameterize ([board (flip-coordinates location-coords (board))]
                   [current-player (toggle-player player)]
                   [current-message (string-join (list "Raised a " (role-at-location location-coords (board))))]
                   [first-turn #f]
                   [partial-turn #f])
      (update-ui)
      (next-event))))

(define (move-src-event location-coords)
  (parameterize ([src-coords location-coords]
                 [current-message (string-join (list
                                                (player-at-location location-coords (board))
                                                (role-at-location location-coords (board))
                                                "selected, choose destination"))]
                 [partial-turn #t])
    (update-ui)
    (next-event)))

(define (wrong-player)
  (parameterize ([current-message "Selected other players piece."])
    (update-ui)
    (next-event)))

(define (handle-button-click location-coords)
  (cond
    ((partial-turn) (finish-move-turn location-coords))
    ((location-hidden? location-coords (board))
     (raise-location location-coords))
    ((eq? (current-player)
          (player-at-location location-coords (board)))
     (move-src-event location-coords))
    (else (wrong-player))
    ))


(define (next-event [continue? #t])
  (let ([button-value (channel-get button-event) ])
    (cond
      (continue? (handle-button-click  button-value))
      (else (exit)))))

(send game-window show #t)

(thread next-event)
