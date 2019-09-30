#lang racket/base
(require racket/list)
(require racket/format)
(require racket/string)

(require "banqi.rkt")

(provide (prefix-out cli: (all-defined-out)) )

(define max-name-len 8)
(define cli-separator-len 91)
(define line-break
  (make-string cli-separator-len #\-))

(define (loc-field-print-padded attr-fn piece)
  (~a (attr-fn piece)
      #:width max-name-len
      #:align 'center
      #:left-pad-string "_"
      #:right-pad-string "_"))

(define (print-row row-list attr-fn)
  (string-join (map attr-fn
                    row-list)
               " | "
               #:before-first "\n|| "
               #:after-last " ||\n"))

(define (role-print-padded piece [show-hidden? #f])
  (loc-field-print-padded (lambda (piece) (role-name piece show-hidden?))
                          piece))

(define (board-row-role-str row-list [show-hidden? #f])
  (print-row row-list (lambda (piece)
                        (role-print-padded piece
                                           show-hidden?))))

(define (player-print-padded piece [show-hidden? #f])
  (loc-field-print-padded (lambda (piece)(player-name piece show-hidden? ))
                          piece))

(define (board-row-player-str row-list [show-hidden? #f])
  (print-row row-list (lambda (piece)
                        (player-print-padded piece
                                             show-hidden?))))


(define (location-revealed-print-padded piece)
  (loc-field-print-padded piece-revealed? piece))

(define (board-row-revealed-str row-list)
  (print-row row-list location-revealed-print-padded))

(define (board-row index board [show-hidden? #f])
  (let ([row (get-row index board)])
    (string-append (board-row-role-str row show-hidden?)
                   (board-row-player-str row show-hidden?))))

(define (strfmt-board board [show-hidden? #f])
  (string-join (flatten
                (map (lambda (i) (board-row i board show-hidden?))
                     (range board-rows)))
               line-break
               #:before-first line-break
               #:after-last line-break))

(define (display-board board [show-hidden? #f])
  (display (strfmt-board board show-hidden?)))
