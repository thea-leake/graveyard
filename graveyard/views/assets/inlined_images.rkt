;; Bitmap files in this directory are by catspook - https://github.com/catspook/graveyard-pics and licensed under the Creative Commons Attribution-NonCommercial 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by-nc/4.0/.

;; Code Copyright 2019 Thea Leake

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;; http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

#lang at-exp racket/base

(provide tile-mappings
         welcome-bitmap
         hidden-tile-text
         selected-image)

(require images/compile-time
         (prefix-in r: "../../models/roles.rkt")
         (prefix-in b: "../../models/board.rkt")
         (prefix-in s: "../image_settings.rkt")
         (only-in racket/list
                  take
                  drop
                  range)
         (for-syntax (prefix-in s: "../image_settings.rkt")
                     (prefix-in r: "../../models/roles.rkt")
                     (prefix-in b: "../../models/board.rkt")
                     racket/base
                     (only-in pict
                              pict->bitmap)
                     (only-in racket/draw
                              read-bitmap)
                     (only-in 2htdp/image
                              scale
                              image-width
                              text
                              text/font
                              above
                              overlay)
                     (only-in racket/list
                              cartesian-product
                              range)
                     (only-in racket/string
                              string-join)))


(begin-for-syntax
  (define image-type-list
    (cons s:hidden
          r:role-hierarchy))

  (define image-type-count
    (length image-type-list))

  (define hidden-tile-text-compiled
    (pict->bitmap
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
      )))

  (define (get-transform image-name)
    (cond
      ((regexp-match #rx"^hidden"
                     image-name)
       (lambda (image)
         (overlay hidden-tile-text-compiled
                  image)))
      (else
       (lambda (image)
         image))))

  (define (transform-image image-name)
    (let* ([image-path (string-append (path->string
                                       (current-load-relative-directory))
                                    image-name)]
           [image (read-bitmap image-path)]
           [img-size (image-width image)]
           [type-transform (get-transform image-name)])
      (pict->bitmap
       (type-transform (scale (/ s:tile-width
                               img-size)
                            image))))))


(define image-type-list
  (cons s:hidden
        r:role-hierarchy))

(define image-type-count
  (length image-type-list))


(define tile-mappings-list
  (compiled-bitmap-list
   (map (lambda (x)
          (transform-image (format "~a-~a.bmp"
                                   (string-downcase (cadr x))
                                   (car x))))
        (cartesian-product (range b:board-rows)
                           image-type-list))))

(define (make-row-hash row)
  (make-immutable-hash
   (map cons
        image-type-list
        (drop (take tile-mappings-list
                    (* (add1 row)
                       image-type-count))
              (* row
                 image-type-count)))))

(define tile-mappings
  (map make-row-hash (range b:board-rows)))


(define welcome-bitmap
  (compiled-bitmap
   (pict->bitmap
    (text/font "Welcome to Queen of the Graveyard!"
               27
               "Goldenrod"
               "Herculanum"
               'decorative
               'normal
               'normal
               #f))))

(define hidden-tile-text
  (compiled-bitmap hidden-tile-text-compiled))

(define selected-image
  (compiled-bitmap
   (pict->bitmap
    (text (string-join (list "----%----"
                             "Selected"
                             "----%----")
                       "\n")
          18
          'Red))))

