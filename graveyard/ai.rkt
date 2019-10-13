#lang racket/base
(require (only-in minikanren
                  ==
                  run*)
         (prefix-in g: "graveyard.rkt"))

(provide start-ai)

(define turn-wait-time .75)

(define (ai-turn state)
  (let ([turns (g:valid-player-turns state)])
    (cond
      ((not (null? (hash-ref turns 'flips)))
       (car (hash-ref turns 'flips )))
      (else (println "GRRR")))))


(define (ai-player chnl)
  (let loop ([continue? #t])
      (let ([message (channel-get chnl)])
        (when message
          (unless (g:turn-src-coords message)
            (sleep turn-wait-time))
          (loop (channel-put chnl (ai-turn message)))))))

(define (start-ai chnl)
  (thread
   (lambda ()
     (ai-player chnl))))
