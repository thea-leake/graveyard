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
         (prefix-in g: "graveyard.rkt")
         (prefix-in c: "colors.rkt"))


(provide tile-width
         tile-height
         welcome-bitmap
         hidden-tile-label
         get-tile-label)

(define tile-width 150)

(define tile-height tile-width)

(define player-role-bar-height 10)

(define hidden "hidden")

(define (get-role-bitmap role row)
  (let* ([path-str (string-join (list "assets/"
                                 (string-downcase role)
                                 "-"
                                 (~a row)
                                 ".png")
                                "")]
         [image (bitmap/file path-str)]
         [img-size (image-width image)]
         [scaled-image (scale (/ tile-width
                                 img-size)
                              image)])
    (cons role
          scaled-image)))

(define (get-row-bitmaps row)
  (make-immutable-hash
   (map (lambda (role)
          (get-role-bitmap role row))
        (cons hidden
              g:role-hierarchy))))

(define tile-mappings
  (map get-row-bitmaps
       (range g:board-rows)))

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
                      (g:position-row coords))
            role))

(define (hidden-tile-label coords)
  (pict->bitmap
   (overlay hidden-tile-text
            (get-tile-mapping hidden
                              coords))))

(define/memo (empty-plot-label coords) ;; memoizing because of last
  (pict->bitmap
   (get-tile-mapping (last g:role-hierarchy)
                     coords)))


(define (player-role-image piece)
  (overlay/align 'center 'bottom
                 (text (g:cell-role piece)
                       player-role-bar-height
                       c:label-blue)
                 (rectangle tile-width player-role-bar-height
                            'solid
                            (c:get-color (string-join
                                          (list (g:cell-player piece)
                                                "Transparent")
                                          "")))))

(define (revealed-base-label piece coords)
  (overlay/align 'center 'bottom
                 (player-role-image piece)
                 (get-tile-mapping (g:cell-role piece)
                                   coords)))

(define/memo (revealed-label piece coords)
  (pict->bitmap
   (revealed-base-label piece
                        coords)))

(define/memo (selected-label piece coords)
  (pict->bitmap
   (overlay/align 'center 'center
                  selected-image
                  (revealed-base-label piece coords))))

;;;;;;
;; Exported image building fns
;;;;;;
(define/memo (tile-init-label coords)
  (empty-plot-label coords))


(define/memo (get-tile-label state piece coords)
  (cond
    ((g:cell-empty? piece) (empty-plot-label coords))
    ((and (equal? coords (g:turn-src-coords state))
          (g:cell-revealed? piece))
     (selected-label piece coords))
    ((g:cell-revealed? piece) (revealed-label piece coords))
    (else
     (hidden-tile-label coords))))
