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
(require (only-in racket/list
                  shuffle)
         (prefix-in g: "graveyard.rkt"))

(provide start-ai)

(define turn-wait-time .75)

;; This contains all the things the computer event loop would want to keep track of or pass on
(struct ai-state
  (turn-response
   next-turn-dest))


(define (choose-flip actions)
  (car (shuffle (g:actions-flips actions))))


(define (choose-locations-easy actions)
  (let ([moves (g:actions-moves actions)]
        [captures (g:actions-captures-thunk actions)])
    (cond
      ((null? moves) (cons (choose-flip actions)
                           #f))
      ((null? (captures))
       (let ([move (car (shuffle moves))])
         (cons (car move)
               (cadr move))))
      (else
       (let ([capture (car (shuffle (captures)))])
         (cons (car capture)
               (cadr capture)))))))


(define (choose-dest prev-turn)
  (ai-state-next-turn-dest prev-turn))


(define (ai-builder choose-locations-fn)
  (lambda (turn prev-turn)
    (cond
      ((g:turn-src-coords turn) (ai-state (choose-dest prev-turn)     ;; turn choice src
                                          #f))                        ;; next turn dest
      (else (let* ([chosen-locations
                    (choose-locations-fn (g:valid-player-turns turn))]
                   [chosen-src (car chosen-locations)]
                   [chosen-dest (cdr chosen-locations)])
              (ai-state chosen-src                                    ;; turn choice src
                        chosen-dest))))))                              ;; next turn dest


(define (ai-player chnl ai-logic)
  (let loop ([message (channel-get chnl)]
             [ai-prev-turn (ai-state #f
                                     #f)])
    (when message
      (sleep turn-wait-time)
      (let ([ai-decision (ai-logic message ai-prev-turn)])
        (channel-put chnl
                     (ai-state-turn-response ai-decision))
        (loop (channel-get chnl)
              ai-decision)))))

(define (start-ai chnl difficulty)
  (let ([ai-logic
         (ai-builder
          (cond
            ((eq? difficulty 'easy) choose-locations-easy)))])
    (thread
     (lambda ()
       (ai-player chnl ai-logic)))))
