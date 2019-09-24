#lang racket/gui
(require racket/gui/base)
(require racket/format)
(require table-panel)
;; using: gen-board board-indexes role-name board-rows board-columns piece-revealed? piece-empty?
(require "banqi.rkt")

(provide (all-defined-out))

(define game-window (new frame% [label "Graveyard"]))
(define start-game-msg (new message%
                            [parent game-window]
                            [label "Welcome to Queen of the Graveyard!"]
                            ))

(define board-container game-window) ;; we'll likely be putting this into a canvas etc.. making it easier to change later

(define board-table
  (new table-panel%
       [parent board-container]
       [border 2]
       [dimensions (list board-rows board-columns)]))

(define init-board (gen-board))

(define (get-button-label piece)
  (cond
    ((piece-empty? piece) (~a "An empty Plot!"))
    ((piece-revealed? piece) (~a "Role:" (role-name piece)
                                 "\n"
                                 "Player: " (player-name piece)))
    (else (~a "Still buried\nClick to raise!"))))

(define (make-button piece)
  (new button%
       [parent board-table]
       [label (get-button-label piece)]))

(define (update-button button-piece)
  (println button-piece)
  (send (car button-piece)
        set-label (get-button-label (cdr button-piece))))

(define button-list
  (map make-button
            init-board))

(define (update-board board)
  (for-each update-button
            (map cons
                 button-list
                 board)))

;; ;; Using parameters here, we're going with a more message passing style for GUI code
;; (define board (make-parameter (gen-board)))

;; (define (build-location-panel row-index column-index row-panel)
;;     (new panel%
;;          [parent row-panel]
;;          [border 2]))

;; (define (add-location-button row-index column-index location-panel)
;;   (new button%
;;        [label (~a "Column" column-index
;;                   "Row" row-index
;;                   #:separator "-")]
;;        [parent location-panel]))

;; (define (build-row-panel row-index)
;;     (new horizontal-panel%
;;           [parent row-container]))

;; (define (build-row row-index)
;;   (let* ([row (build-row-panel row-index)]
;;         [build-location (lambda (column-index)
;;                           (add-location-button row-index
;;                                                column-index
;;                                                (build-location-panel row-index
;;                                                                      column-index
;;                                                                      row)))])
;;     (map build-location (range board-columns))))

;; (define rows
;;   (map build-row (range board-rows)))

(send game-window show #t)
