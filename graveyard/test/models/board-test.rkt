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

(define-test-suite gen-board-tests
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

    (test-case "Check gen-board generates 32 pieces"
      (check-eq? board-length
                32))

    (test-case "Check no pieces are revealed when board generated"
      (check-eq? revealed-piece-count
                0))

    (test-case "Check players have 16 pieces"
      (check-eq? player1-piece-count
                16))))

(run-tests gen-board-tests)

(define-test-suite get-coords-from-index-tests
  "Test get-coords-from-index"
  (let ([top-left-index 0]

        [bottom-right-index 31])

    (test-case "Correct coords returned for top left location index"
      (check-equal? (b:get-coords-from-index top-left-index)
                   (b:position 0 0)))

    (test-case "Correct coords returned for bottom right location index"
      (check-equal? (b:get-coords-from-index bottom-right-index)
                   (b:position 7 3)))

    (test-case "Verify coords returned are same instance"
      (check-eq? (b:get-coords-from-index bottom-right-index)
                 (b:get-coords-from-index bottom-right-index)))))

(run-tests get-coords-from-index-tests)

(define-test-suite get-index-from-coordinates-test
  "Test get-index-from-coordinates"
  (let ([top-left (b:position 0 0)]

        [bottom-right (b:position 7 3)])

    (test-case "Correct index returned for top left coordinates"
      (check-eq? (b:get-index-from-coordinates top-left)
                0))

    (test-case "Correct index returned for bottom right coordinates"
      (check-eq? (b:get-index-from-coordinates bottom-right)
                31))))

(run-tests get-index-from-coordinates-test)

(define-test-suite coords-out-of-range-tests
  "Test coords-out-of-range"
  (let ([top-left (b:position 0 0)]

        [bottom-right (b:position 7 3)]

        [y-axis-off (b:position 7 4)]

        [x-axis-off (b:position 9 2)]

        [both-axi-off (b:position -1 -3)])

    (test-case "Check top left cords on board"
      (check-false (b:coords-out-of-range? top-left)))

    (test-case "Check bottom right coords on board"
      (check-false (b:coords-out-of-range? bottom-right)))

    (test-case "Check y axis off board"
      (check-true (b:coords-out-of-range? y-axis-off)))

    (test-case "Check x axis off board"
      (check-true (b:coords-out-of-range? x-axis-off)))

    (test-case "Check both axi off board"
      (check-true (b:coords-out-of-range? both-axi-off)))))

(run-tests coords-out-of-range-tests)

(define (coord-sort c1 c2)
  (> (b:get-index-from-coordinates c2)
      (b:get-index-from-coordinates c1)))

(define-test-suite coords-row-column-test
  "Test getting all coordinates in row and column of location"
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
                      coord-sort)))

(run-tests coords-row-column-test)
