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
                  shuffle
                  filter-not
                  empty?
                  filter-map)
         (prefix-in g: "graveyard.rkt")
         (prefix-in u: "utils.rkt"))

(provide start-ai)

(define turn-wait-time .75)

;; This contains all the things the computer event loop would want to keep track of or pass on
(struct ai-state
  (turn-response
   next-turn-dest))


(define (choose-random-flip locations)
  (car (shuffle (g:actions-flips locations))))

(define (choose-random-move moves)
  (car (shuffle moves)))


(define (choose-locations-easy turn)
  (let* ([actions (g:valid-player-turns turn)]
         [moves (g:actions-moves actions)]
         [captures (g:actions-captures-thunk actions)])
    (cond
      ((null? moves) (cons (choose-random-flip actions)
                           '(#f)))
      ((null? (captures)) ;; memoized lambda
       (choose-random-move moves))
      (else
       (choose-random-move (captures))))))


(define (build-move-checker turn)
  (lambda (moves)
    (foldl (lambda (piece-moves safe-moves)
             (let* ([src (car piece-moves)]
                    [dests (cdr piece-moves)]
                    [safe-dests (filter-not (lambda (dest)
                                              (g:unsafe-move? turn
                                                              src
                                                              dest))
                                            dests)])
               (if (not (empty? safe-dests))
                   (cons (cons src
                               safe-dests)
                         safe-moves)
                   safe-moves)))
           '()
           moves)))


(define (choose-locations-medium turn)
  (let* ([actions (g:valid-player-turns turn)]
         [moves-safety-checker (build-move-checker turn)]
         [moves (g:actions-moves actions)]
         [safe-moves (u:inspect (moves-safety-checker moves) #:header "## Safe moves")]
         [captures ((g:actions-captures-thunk actions))]
         [safe-captures (u:inspect (moves-safety-checker captures) #:header "## Safe Captures")])
    (cond
      ((null? moves) (cons (choose-random-flip actions)
                           '(#f)))
      ((not (empty? safe-captures))
       (choose-random-move safe-captures))
      ((not (empty? safe-moves))
       (choose-random-move safe-moves))
      ((null? captures)
       (choose-random-move moves))
      (else
       (choose-random-move captures)))))


(define (choose-dest prev-turn)
  (ai-state-next-turn-dest prev-turn))


(define (ai-builder choose-locations-fn)
  (lambda (turn prev-turn)
    (cond
      ((g:turn-src-coords turn) (ai-state (choose-dest prev-turn)     ;; turn choice src
                                          #f))                        ;; next turn dest
      (else (let* ([chosen-locations
                    (choose-locations-fn turn )]
                   [chosen-src (car chosen-locations)]
                   [chosen-dest (cadr chosen-locations)])
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
          (case difficulty
            ('easy choose-locations-easy)
            ('medium choose-locations-medium)))])
    (thread
     (lambda ()
       (ai-player chnl ai-logic)))))
