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
(define player1 (car players))
(define player2 (cdr players))
(define empty-role "Empty")

(define player1-zombie
  (cell player1
        #t
        elephant
        #f))

(define player1-lich
  (cell player1
        #t
        leader
        #f))

(define player2-vampire
  (cell player2
        #t
        advisor
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


(define move-with-unsafe-capture
  (list   empty-location    empty-location    empty-location  player1-zombie  empty-location     empty-location    empty-location     empty-location
          empty-location    empty-location    player1-lich    player1-zombie  player2-vampire    empty-location    empty-location     empty-location
          empty-location    empty-location    empty-location  player1-zombie  empty-location     empty-location    empty-location     empty-location
          empty-location    empty-location    empty-location  empty-location  empty-location     empty-location    empty-location     empty-location))

(define game-state-unsafe-capture
  (turn move-with-unsafe-capture
        player2
        "" #f #f #f #f))


(define unsafe-move?-tests
  (test-suite "Tests checking whether moves are safe or not"
              (check-equal? (unsafe-move? game-state-safe-capture
                                          (position 4 1)
                                          (position 3 1))
                            #f)
              (check-equal? (unsafe-move? game-state-unsafe-capture
                                          (position 4 1)
                                          (position 3 1))
                            (list (position 2 1)))))

(run-tests unsafe-move?-tests)
