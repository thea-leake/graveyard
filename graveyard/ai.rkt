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

(define (choose-flip actions)
  (car (shuffle (g:actions-flips actions))))


(define (choose-src actions)
  (let ([moves (g:actions-moves actions)]
        [captures (g:actions-captures-thunk actions)]) ;; memoized lambda
    (cond
      ((null? moves) (choose-flip actions))
      ((null? (captures))
       (caar (shuffle moves)))
      (else
       (caar (shuffle (captures)))))))

(define (choose-dest src actions)
  (let* ([capture-coords (findf (lambda (x)
                                  (equal? src (car x)))
                                ((g:actions-captures-thunk actions)))])
    (cond
      (capture-coords
       (car (shuffle (cdr capture-coords))))
      (else
       (car (shuffle (cdr (findf (lambda (x)
                               (equal? src (car x)))
                             (g:actions-moves actions)))))))))

(define (ai-turn state)
  (let* ([actions (g:valid-player-turns state)]
         [src (g:turn-src-coords state)])
    (cond
      (src (choose-dest src actions))
      (else (choose-src actions)))))


(define (ai-player chnl)
  (let loop ([message (channel-get chnl)])
    (when message
      (sleep turn-wait-time)
      (channel-put chnl (ai-turn message))
      (loop (channel-get chnl)))))

(define (start-ai chnl)
  (thread
   (lambda ()
     (ai-player chnl))))
