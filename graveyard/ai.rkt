#lang racket/base
(require (prefix-in g: "graveyard.rkt"))

(provide ai-turn)

(define (ai-turn state)
  (let ([turns (g:valid-player-turns state)])
    (println turns)
    (cond
      ((not (null? (hash-ref turns 'flips)))
       (car (hash-ref turns 'flips ))))))
