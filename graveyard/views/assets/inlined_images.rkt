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

#lang racket/base

;; (provide tile-mappings)

(require images/compile-time
         (for-syntax "../image_settings.rkt"
                     racket/base
                     (only-in pict
                              pict->bitmap)
                     (only-in racket/draw
                              read-bitmap)
                     (only-in 2htdp/image
                              scale
                              image-width)))

(provide tile-mappings)

;; This is kind of messy, but it allows inlining of the images into the
;; bytecode, so the images can be included inside the executable.
;; something more dynamic would require the images to be present at run time.

(begin-for-syntax
  (define (transform-image image-name)
    (let* ([image-path (string-append (path->string
                                       (current-load-relative-directory))
                                    image-name)]
           [image (read-bitmap image-path)]
           [img-size (image-width image)])
      (pict->bitmap (scale (/ tile-width
                 img-size)
              image)))))


(define tile-mappings
  (list
   (make-immutable-hash
    (list
     (cons "Empty" (compiled-bitmap (transform-image "empty-0.bmp")))
     (cons "Ghoul" (compiled-bitmap (transform-image "ghoul-0.bmp")))
     (cons "hidden" (compiled-bitmap (transform-image "hidden-0.bmp")))
     (cons "Lich" (compiled-bitmap (transform-image "lich-0.bmp")))
     (cons "Poltergeist" (compiled-bitmap (transform-image "poltergeist-0.bmp")))
     (cons "Skeleton" (compiled-bitmap (transform-image "skeleton-0.bmp")))
     (cons "Vampire" (compiled-bitmap (transform-image "vampire-0.bmp")))
     (cons "Wraith" (compiled-bitmap (transform-image "wraith-0.bmp")))
     (cons "Zombie" (compiled-bitmap (transform-image "zombie-0.bmp")))))
   (make-immutable-hash
    (list
     (cons "Empty" (compiled-bitmap (transform-image "empty-1.bmp")))
     (cons "Ghoul" (compiled-bitmap (transform-image "ghoul-1.bmp")))
     (cons "hidden" (compiled-bitmap (transform-image "hidden-1.bmp")))
     (cons "Lich" (compiled-bitmap (transform-image "lich-1.bmp")))
     (cons "Poltergeist" (compiled-bitmap (transform-image "poltergeist-1.bmp")))
     (cons "Skeleton" (compiled-bitmap (transform-image "skeleton-1.bmp")))
     (cons "Vampire" (compiled-bitmap (transform-image "vampire-1.bmp")))
     (cons "Wraith" (compiled-bitmap (transform-image "wraith-1.bmp")))
     (cons "Zombie" (compiled-bitmap (transform-image "zombie-1.bmp")))))
   (make-immutable-hash
    (list
     (cons "Empty" (compiled-bitmap (transform-image "empty-2.bmp")))
     (cons "Ghoul" (compiled-bitmap (transform-image "ghoul-2.bmp")))
     (cons "hidden" (compiled-bitmap (transform-image "hidden-2.bmp")))
     (cons "Lich" (compiled-bitmap (transform-image "lich-2.bmp")))
     (cons "Poltergeist" (compiled-bitmap (transform-image "poltergeist-2.bmp")))
     (cons "Skeleton" (compiled-bitmap (transform-image "skeleton-2.bmp")))
     (cons "Vampire" (compiled-bitmap (transform-image "vampire-2.bmp")))
     (cons "Wraith" (compiled-bitmap (transform-image "wraith-2.bmp")))
     (cons "Zombie" (compiled-bitmap (transform-image "zombie-2.bmp")))))
   (make-immutable-hash
    (list
     (cons "Empty" (compiled-bitmap (transform-image "empty-3.bmp")))
     (cons "Ghoul" (compiled-bitmap (transform-image "ghoul-3.bmp")))
     (cons "hidden" (compiled-bitmap (transform-image "hidden-3.bmp")))
     (cons "Lich" (compiled-bitmap (transform-image "lich-3.bmp")))
     (cons "Poltergeist" (compiled-bitmap (transform-image "poltergeist-3.bmp")))
     (cons "Skeleton" (compiled-bitmap (transform-image "skeleton-3.bmp")))
     (cons "Vampire" (compiled-bitmap (transform-image "vampire-3.bmp")))
     (cons "Wraith" (compiled-bitmap (transform-image "wraith-3.bmp")))
     (cons "Zombie" (compiled-bitmap (transform-image "zombie-3.bmp")))))))
