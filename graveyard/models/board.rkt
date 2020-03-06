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

#lang typed/racket/base #:with-refinements

(provide board-coordinates
         board-columns
         board-rows
         board-indexes
         get-index-from-coordinates
         get-coords-from-index
         coords-out-of-range?
         coords-row-columns
         gen-board
         Position
         Row
         Column
         Index
         (struct-out position)
         none-position)


(require (only-in tmemoize
                  memoized
                  memoize
                  )
         (prefix-in r: "roles.rkt"))

(require/typed racket/list
  [shuffle (-> (Listof r:cell)
               (Listof r:cell))])



(define-type None False)

(define none : None #f)


(define board-rows : Integer
  4)

(define board-columns : Integer
  8)

(define location-count : Integer
  (* board-columns
     board-rows))

(define-type Column
  (Refine [n : Integer]
          (and (> 8 n)
               (<= 0 n))))


(define-type Row
  (Refine [n : Integer]
          (and (> 4 n)
               (<= 0 n))))

(define-type Dimension (U Column Row))

(define-type Index
  (Refine [n : Integer]
          (and (> 32 n)
               (<= 0 n))))


(require/typed racket/list
  [range (-> Integer (Listof Index))])

(define board-indexes : (Listof Index)
  (range location-count))

(struct position
  ([column : Column]
   [row : Row])
  #:transparent)

(define-type Position (U position None))
(define none-position : Position #f)

(memoized
 (: get-index-from-coordinates (-> Position Index))
 (define (get-index-from-coordinates coords?)
   (case coords?
     [(none-position) (error "No index for None position")]
     [else (let* ([coords (cast coords? position)]
               [x : Column  (position-column coords)]
               [y : Row (position-row coords)])
          (cast (+ (* y board-columns)
                   x)
                Index))])))

(memoized
 (: get-coords-from-index (-> Index position))
 (define (get-coords-from-index index)
   (let ([x : Column (cast (remainder index
                                      board-columns)
                           Column)]
         [y : Row (cast (quotient index
                                  board-columns)
                        Row)])
     (position x y))))


(define board-coordinates : (Listof position)
  (map get-coords-from-index board-indexes))

(memoized
 (: index-in-range? (-> Index Boolean))
 (define (index-in-range? index)
   (and (<= 0 index)
        (> location-count index))))

(memoized
 (: coords-in-range? (-> Position Boolean))
 (define (coords-in-range? coords?)
   (case coords?
     [(none-position) #f]
     [else (let ([coords (cast coords? position)])
             (and (index-in-range? (get-index-from-coordinates coords))
                  (< (position-column coords) board-columns)
                  (< (position-row coords) board-rows)))])))


(memoized
 (: coords-out-of-range? (-> Position Boolean))
 (define (coords-out-of-range? coords)
   (not (coords-in-range? coords))))

(memoized
 (: coords-row-columns (-> Position (Listof position)))
 (define (coords-row-columns coords?)
   (case coords?
     [(none-position) '()]
     [else
      (let* ([coords (cast coords? position)]
             [check : (-> (-> position Dimension)
                         position
                         Boolean)
                   (lambda (
                            [fn : (-> position Dimension)]
                            [check-coords : position])
                     (= (fn check-coords)
                        (fn coords)))])
        (filter (lambda ([check-coords : position])
                  (or (check position-row check-coords)
                      (check position-column check-coords)))
                board-coordinates))])))

(define (gen-board)
  (shuffle (append (r:player-roles (car r:players))
                   (r:player-roles (cdr r:players)))))
