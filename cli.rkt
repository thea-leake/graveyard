#lang racket/base
(require racket/include)
(require racket/list)
(require racket/format)
(require racket/string)

(require "banqi.rkt")

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

(define (role-reveal-print-padded piece)
  (loc-field-print-padded role-name piece))

(define (print-row row-list attr-fn)
  (string-join (map attr-fn
                    row-list)
               " | "
               #:before-first "\n|| "
               #:after-last " ||\n"))

(define (board-reveal-row-role-str row-list)
  (print-row row-list role-reveal-print-padded))

(define (player-reveal-print-padded piece)
  (loc-field-print-padded player-name piece))

(define (board-reveal-row-player-str row-list)
  (print-row row-list player-reveal-print-padded))


(define (location-revealed-print-padded piece)
  (loc-field-print-padded location-revealed? piece))

(define (board-reveal-row-revealed-str row-list)
  (print-row row-list location-revealed-print-padded))

(define (board-reveal-row index board)
  (let ([row (get-row index board)])
    (string-append (board-reveal-row-role-str row)
                   (board-reveal-row-player-str row)
                   (board-reveal-row-revealed-str row))))

(define (strfmt-board-reveal board)
  (string-join (flatten
                (map (lambda (i) (board-reveal-row i board))
                     (range board-rows)))
               line-break
               #:before-first line-break
               #:after-last line-break))

(define (display-board-reveal board)
  (display (strfmt-board-reveal board)))
