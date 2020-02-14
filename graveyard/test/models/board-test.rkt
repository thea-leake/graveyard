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

(require rackunit
         rackunit/text-ui
         (only-in racket/list
                  take)
         "../../utils.rkt"
         (prefix-in r: "../../models/roles.rkt")
         (prefix-in b: "../../models/board.rkt"))

(define player1 (car r:players))
(define player2 (cdr r:players))


(define test-board (b:gen-board))

(define gen-board-tests
  (test-suite
   "Test board generation"
   (let ([board-length (length test-board)]
         [revealed-piece-count (length
                                (filter (lambda (x)
                                          (r:cell-revealed? x))
                                        test-board))]
         [player1-piece-count (length
                               (filter (lambda (x)
                                         (eq? (r:cell-player x)
                                              player1))
                                       test-board))])
     (check-eq? board-length
                  32)
     (check-eq? revealed-piece-count
                  0)
     (check-eq? player1-piece-count
                  16))))

(run-tests gen-board-tests)

(define board-location-tests
  (test-suite
   "Test board location functions"

   (test-case
     "Test get-index-from-coordinates"
     (let ([top-left (b:position 0 0)]
           [bottom-right (b:position 7 3)])
      (check-eq? (b:get-index-from-coordinates top-left)
                 0)
      (check-eq? (b:get-index-from-coordinates bottom-right)
                 31)))

   (test-case
     "Test get-coords-from-index"
     (let ([top-left-index 0]
           [bottom-right-index 31])
       (check-equal? (b:get-coords-from-index top-left-index)
                     (b:position 0 0))
       (check-equal? (b:get-coords-from-index bottom-right-index)
                     (b:position 7 3))
       (test-case
         "Verify coords returned are same instance"
         (check-eq? (b:get-coords-from-index bottom-right-index)
                    (b:get-coords-from-index bottom-right-index)))))

   (test-case
     "Test coords-out-of-range"
     (let ([top-left (b:position 0 0)]
           [bottom-right (b:position 7 3)]
           [off-map-examples (list
                              (b:position 7 4)
                              (b:position -1 -3)
                              (b:position 9 2))])
       (check-false (b:coords-out-of-range? top-left))
       (check-false (b:coords-out-of-range? bottom-right))
       (for-each check-true
            (map b:coords-out-of-range?
                    off-map-examples))))))

(run-tests board-location-tests)

(define (coord-sort c1 c2)
  (> (b:get-index-from-coordinates c2)
      (b:get-index-from-coordinates c1)))

(define coords-row-column-test
  (test-suite "Test getting all coordinates in row and column of location"
              (check-equal? (sort (b:coords-row-columns (b:position 2 1))
                                  coord-sort)
                            (sort (list
                                   (b:position 2 0)
                                   (b:position 0 1)
                                   (b:position 1 1)
                                   (b:position 2 1)
                                   (b:position 3 1)
                                   (b:position 4 1)
                                   (b:position 6 1)
                                   (b:position 5 1)
                                   (b:position 7 1)
                                   (b:position 2 2)
                                   (b:position 2 3))
                                  coord-sort))))

(run-tests coords-row-column-test)
