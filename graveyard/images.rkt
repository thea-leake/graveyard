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
                  bitmap/file
                  scale
                  image-width)
         (prefix-in b: "models/board.rkt")
         (prefix-in g: "models/graveyard.rkt")
         (prefix-in r: "models/roles.rkt")
         (prefix-in c: "colors.rkt")
         (prefix-in i: "assets/inlined_images.rkt")
         (prefix-in s: "image_settings.rkt"))


(provide welcome-bitmap
         hidden-tile-label
         get-tile-label)

(define tile-width 150)

(define tile-height tile-width)

(define player-role-bar-height 10)

(define hidden "hidden")


(define tile-mappings i:tile-mappings)

;;;;;;;
;; base images
;;;;;;


(define welcome-bitmap
  (pict->bitmap (text "Welcome to Queen of the Graveyard!"
                      27
                      "Goldenrod")))



(define hidden-tile-text
  (above
   (text (string-join (list
                       "   Still buried     "
                       " Click to raise! "
                       "      @>-`-,-      "
                       )
                      "\n")
         13
         "LightSlateGray")
   (text " ####-#### "
         14
         "LightSlateGray")
   ))

(define selected-image
  (text (string-join (list "----%----"
                           "Selected"
                           "----%----")
                     "\n")
        18
        'Red))



;;;;;;;;;
;; Image building fns
;;;;;;;;;


(define/memo (get-tile-mapping role coords)
  (hash-ref (list-ref tile-mappings
                      (b:position-row coords))
            role))

(define/memo (hidden-tile-label coords)
  (pict->bitmap
   (overlay hidden-tile-text
            (get-tile-mapping hidden
                              coords))))

(define/memo (empty-plot-label coords) ;; memoizing because of last
  (pict->bitmap
   (get-tile-mapping (last r:role-hierarchy)
                     coords)))


(define (player-role-image role player)
  (overlay/align 'center 'bottom
                 (text role
                       player-role-bar-height
                       c:label-blue)
                 (rectangle tile-width player-role-bar-height
                            'solid
                            (c:get-color (string-join
                                          (list player
                                                "Transparent")
                                          "")))))

(define/memo (revealed-base-label role player coords)
  (overlay/align 'center 'bottom
                 (player-role-image role player)
                 (get-tile-mapping role
                                   coords)))

(define/memo (revealed-label role player coords)
  (pict->bitmap
   (revealed-base-label role
                        player
                        coords)))

(define/memo (selected-label role player coords)
  (pict->bitmap
   (overlay/align 'center 'center
                  selected-image
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
