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
(require (only-in memoize
                  define/memo
                  memo-lambda)
         "definitions.rkt")


(provide board-coordinates
         get-index-from-coordinates
         get-coords-from-index
         coords-out-of-range?
         coords-row-columns)

(define/memo (get-index-from-coordinates coords)
  (let ([x (position-column coords)]
        [y (position-row coords)])
    (+ (* y board-columns)
       x)))

(define/memo (get-coords-from-index index)
  (let ([x (remainder index
                      board-columns)]
        [y (quotient index
                     board-columns)])
    (position x y)))


(define board-coordinates (map get-coords-from-index board-indexes))

(define/memo (index-in-range? index)
  (and (<= 0 index)
       (> location-count index)))

(define/memo (coords-in-range? coords)
  (and (index-in-range? (get-index-from-coordinates coords))
       (< (position-column coords) board-columns)
       (< (position-row coords) board-rows)))


(define/memo (coords-out-of-range? coords)
  (not (coords-in-range? coords)))

(define/memo (coords-row-columns coords)
  (let ([check (lambda (fn check-coords)
                 (= (fn check-coords)
                    (fn coords)))])
    (filter (lambda (check-coords)
              (or (check position-row check-coords)
                  (check position-column check-coords)))
            board-coordinates)))
