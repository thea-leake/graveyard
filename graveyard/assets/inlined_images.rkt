;; Art Copyright 2019 Trina Rutz Creative Commons

;; Code Copyright 2019 Thea Leake
;; Code License:
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

(provide tile-mappings)

(require mrlib/include-bitmap
         (only-in 2htdp/image
                  scale
                  image-width)
         (prefix-in s: "../image_settings.rkt"))

;; This is kind of messy, but it allows inlining of the images into the
;; bytecode, so the images can be included inside the executable.
;; something more dynamic would require the images to be present at run time.

(define (transform-image image)
  (let ([img-size (image-width image)])
    (scale (/ s:tile-width
              img-size)
           image)))

(define tile-mappings
  (list
   (make-immutable-hash
    (list
     (cons "Empty" (transform-image (include-bitmap "empty-0.bmp")))
     (cons "Ghoul" (transform-image (include-bitmap "ghoul-0.bmp")))
     (cons "hidden" (transform-image (include-bitmap "hidden-0.bmp")))
     (cons "Lich" (transform-image (include-bitmap "lich-0.bmp")))
     (cons "Poltergeist" (transform-image (include-bitmap "poltergeist-0.bmp")))
     (cons "Skeleton" (transform-image (include-bitmap "skeleton-0.bmp")))
     (cons "Vampire" (transform-image (include-bitmap "vampire-0.bmp")))
     (cons "Wraith" (transform-image (include-bitmap "wraith-0.bmp")))
     (cons "Zombie" (transform-image (include-bitmap "zombie-0.bmp")))))
   (make-immutable-hash
    (list
     (cons "Empty" (transform-image (include-bitmap "empty-1.bmp")))
     (cons "Ghoul" (transform-image (include-bitmap "ghoul-1.bmp")))
     (cons "hidden" (transform-image (include-bitmap "hidden-1.bmp")))
     (cons "Lich" (transform-image (include-bitmap "lich-1.bmp")))
     (cons "Poltergeist" (transform-image (include-bitmap "poltergeist-1.bmp")))
     (cons "Skeleton" (transform-image (include-bitmap "skeleton-1.bmp")))
     (cons "Vampire" (transform-image (include-bitmap "vampire-1.bmp")))
     (cons "Wraith" (transform-image (include-bitmap "wraith-1.bmp")))
     (cons "Zombie" (transform-image (include-bitmap "zombie-1.bmp")))))
   (make-immutable-hash
    (list
     (cons "Empty" (transform-image (include-bitmap "empty-2.bmp")))
     (cons "Ghoul" (transform-image (include-bitmap "ghoul-2.bmp")))
     (cons "hidden" (transform-image (include-bitmap "hidden-2.bmp")))
     (cons "Lich" (transform-image (include-bitmap "lich-2.bmp")))
     (cons "Poltergeist" (transform-image (include-bitmap "poltergeist-2.bmp")))
     (cons "Skeleton" (transform-image (include-bitmap "skeleton-2.bmp")))
     (cons "Vampire" (transform-image (include-bitmap "vampire-2.bmp")))
     (cons "Wraith" (transform-image (include-bitmap "wraith-2.bmp")))
     (cons "Zombie" (transform-image (include-bitmap "zombie-2.bmp")))))
   (make-immutable-hash
    (list
     (cons "Empty" (transform-image (include-bitmap "empty-3.bmp")))
     (cons "Ghoul" (transform-image (include-bitmap "ghoul-3.bmp")))
     (cons "hidden" (transform-image (include-bitmap "hidden-3.bmp")))
     (cons "Lich" (transform-image (include-bitmap "lich-3.bmp")))
     (cons "Poltergeist" (transform-image (include-bitmap "poltergeist-3.bmp")))
     (cons "Skeleton" (transform-image (include-bitmap "skeleton-3.bmp")))
     (cons "Vampire" (transform-image (include-bitmap "vampire-3.bmp")))
     (cons "Wraith" (transform-image (include-bitmap "wraith-3.bmp")))
     (cons "Zombie" (transform-image (include-bitmap "zombie-3.bmp")))))))
