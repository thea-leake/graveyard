#lang racket/base

(require rackunit
         rackunit/text-ui
         "graveyard.rkt")


;; Test gen-board - check tests working
(define gen-board-tests
  (test-suite "Test board generation"
              (check-equal? (length (gen-board)) 32)))

(run-tests gen-board-tests)

;; Unsafe-move? tests 
(define player1 "Purple")
(define player2 "Orange")
(define empty-role "Empty")

(define player1-zombie
  (cell player1
        #t
        "Zombie"
        #f))

(define player2-vampire
  (cell player2
        #t
        "Vampire"
        #f))

(define empty-location
  (cell #f         ;; player
        #t         ;; revealed
        empty-role ;; role
        #t))       ;; empty

(define move-with-safe-capture
  (list   empty-location    empty-location    empty-location  player1-zombie  empty-location     empty-location    empty-location     empty-location
          empty-location    empty-location    player1-zombie  player1-zombie  player2-vampire    empty-location    empty-location     empty-location
          empty-location    empty-location    empty-location  player1-zombie  empty-location     empty-location    empty-location     empty-location
          empty-location    empty-location    empty-location  empty-location  empty-location     empty-location    empty-location     empty-location))
(define game-state-safe-capture
  (turn move-with-safe-capture
        player2
        "" #f #f #f #f))


(define unsafe-move?-tests
  (test-suite "Tests checking whether moves are safe or not"
              (check-equal? (unsafe-move? game-state-safe-capture
                                          (position 4 1)
                                          (position 3 1))
                            #f)))

(run-tests unsafe-move?-tests)
