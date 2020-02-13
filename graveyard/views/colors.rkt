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

(provide dark-green
         dark-purple-taup
         purple
         orange
         purple-transparent
         orange-transparent
         label-blue
         get-color
         electric-ultramarine)

(require (only-in racket/draw
                  make-color)
         (only-in 2htdp/image
                  color))

(define dark-green
  (make-color 0 100 0))


(define dark-purple-taup
  (make-color 75 65 79))

(define electric-ultramarine
  (make-color 85 0 255))

(define orange
  (color 227 112 0))

(define purple
  (color 217 25 255))

;; border colors..

(define border-opaqueness 127)
(define orange-transparent
  (color 227 112 0 border-opaqueness))

(define purple-transparent
  (color 217 25 255 border-opaqueness))

(define label-blue
  (color 35 64 153))

(define color-mappings
  (make-immutable-hash
   (list (cons "Purple" purple)
         (cons "Orange" orange)
         (cons "PurpleTransparent" purple-transparent)
         (cons "OrangeTransparent" orange-transparent))))

(define (get-color key)
  (hash-ref color-mappings key))
