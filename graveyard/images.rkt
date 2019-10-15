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
         (only-in memoize
                  define/memo)
         (only-in racket/string
                  string-join)
         (only-in memoize
                  define/memo)
         (only-in pict
                  pict->bitmap)
         (only-in 2htdp/image
                  above
                  text
                  overlay
                  rectangle)
         (prefix-in g: "graveyard.rkt")
         (prefix-in c: "colors.rkt"))


(provide tile-width
         tile-height
         welcome-bitmap
         hidden-tile-label
         get-tile-label)



(define tile-width 150)

(define tile-height tile-width)


;;;;;;;
;; base images
;;;;;;


(define welcome-bitmap
  (pict->bitmap (text "Welcome to Queen of the Graveyard!"
                      27
                      "Goldenrod")))



(define tile-background
  (above (rectangle (+ 25 tile-width )
                    (* tile-height 0.75)
                    "solid"
                    "Blue")
         (rectangle (+ 25 tile-width )
                    (* tile-height 0.25)
                    "solid"
                    "MediumForestGreen")))


(define hidden-tile-text
  (above (text (string-join  (list "   --------------"
                                   "/  Still buried   \\"
                                   "|Click to raise!|"
                                   "|     @>-`-,-     |"
                                   )
                             "\n")
               15
               "LightSlateGray")
         (text "| ####-#### |"
               16
               "LightSlateGray")
         (text (make-string 12 #\")
               25
               'MediumForestGreen)))

(define hidden-tile-label
  (pict->bitmap
   (overlay hidden-tile-text
            tile-background)))

(define empty-plot-label
  (let ([rubble (text "%&%*%&@&*%$@%"
                      12
                      'brown)])
    (pict->bitmap
     (overlay (above (text "\n\n\n\nAn Empty Plot!\n"
                           15
                           'black)
                     rubble)
              tile-background))))


(define selected-image
  (text (string-join (list "      ----%----  "
                           "[xx|=selected=>")
                     "\n")
        12
        'Goldenrod))

;;;;;;;;;
;; Image building fns
;;;;;;;;;

(define (add-tile-background image)
  (pict->bitmap
   (overlay image
            tile-background)))


(define/memo (base-revealed-label piece)
  (text (g:role-name piece)
        25
        (g:player-name piece)))

(define/memo (revealed-label piece)
  (add-tile-background
   (base-revealed-label piece)))


(define/memo (selected-label piece)
  (add-tile-background
   (above (base-revealed-label piece)
          selected-image)))

;;;;;;
;; Exported image building fns
;;;;;;


(define/memo (get-tile-label state piece coords)
  (cond
    ((g:piece-empty? piece) empty-plot-label)
    ((and (equal? coords (g:turn-src-coords state))
          (g:piece-revealed? piece))
     (selected-label piece))
    ((g:piece-revealed? piece) (revealed-label piece))
    (else hidden-tile-label)))

