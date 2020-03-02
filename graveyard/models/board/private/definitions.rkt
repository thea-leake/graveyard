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

#lang typed/racket/base
(require (prefix-in r: "../../roles/roles.rkt"))


(provide board-columns
         board-rows
         board-indexes
         location-count
         (struct-out position)
         Position)


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
