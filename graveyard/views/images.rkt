;; Copyright 2019-2021 Thea Leake

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


(require (only-in racket/format
                  ~a)
         (only-in racket/string
                  string-join)
         (only-in racket/list
                  range
                  last)
         (only-in memoize
                  define/memo)
         (only-in pict
                  pict->bitmap)
         (only-in 2htdp/image
                  above
                  text
                  overlay/align
                  overlay
                  rectangle
                  image-width)
         (prefix-in b: "../models/board.rkt")
         (prefix-in g: "../models/graveyard.rkt")
         (prefix-in r: "../models/roles.rkt")
         (prefix-in c: "colors.rkt")
         (prefix-in i: "inlined_images.rkt")
         (prefix-in s: "image_settings.rkt"))


(provide welcome-bitmap
         hidden-tile-label
         get-tile-label)


;; as we're passing this on to views
(define welcome-bitmap i:welcome-bitmap)


;;;;;;;;;
;; Image building fns
;;;;;;;;;


(define/memo (get-tile-mapping role coords)
  (hash-ref (list-ref i:tile-mappings
                      (b:position-row coords))
            role))

(define/memo (hidden-tile-label coords)
  (get-tile-mapping s:hidden
                    coords))

(define/memo (empty-plot-label coords) ;; memoizing because of last
  (get-tile-mapping (last r:role-hierarchy)
                    coords))


(define (player-role-image role player)
  (overlay/align 'center 'bottom
                 (text role
                       s:player-role-bar-height
                       c:label-blue)
                 (rectangle s:tile-width s:player-role-bar-height
                            'solid
                            (c:get-color (string-join
                                          (list player
                                                "Transparent")
                                          "")))))

(define/memo (revealed-base-label role player coords)
  (pict->bitmap
   (overlay/align 'center 'bottom
                  (player-role-image role player)
                  (get-tile-mapping role
                                    coords))))

(define/memo (revealed-label role player coords)
  (revealed-base-label role
                       player
                       coords))

(define/memo (selected-label role player coords)
  (pict->bitmap
   (overlay/align 'center 'center
                  i:selected-image
                  (revealed-base-label role player coords))))

;;;;;;
;; Exported image building fns
;;;;;;
(define/memo (tile-init-label coords)
  (empty-plot-label coords))


(define (get-tile-label state piece coords)
  (let ([role (r:cell-role piece)]
        [player (r:cell-player piece)])(cond
     ((r:cell-empty? piece) (empty-plot-label coords))
     ((and (equal? coords (g:turn-src-coords state))
           (r:cell-revealed? piece))
      (selected-label role player coords))
     ((r:cell-revealed? piece) (revealed-label role player coords))
     (else
      (hidden-tile-label coords)))))
