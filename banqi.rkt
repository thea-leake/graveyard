(require racket/list)

(define player-start-roles
  (flatten
   (list (make-list 2 "General")
         (make-list 2 "Advisor")
         (make-list 2 "Elephant")
         (make-list 2 "Chariot")
         (make-list 2 "Horse")
         (make-list 5 "Soldier")
         (make-list 2 "Cannon"))))

(define (mkpiece player role)
  (hash 'player player
        'revealed #f
        'role role))

(define player1-roles
  (map (lambda (role) (mkpiece 1 role))
       player-start-roles))

(define player2-roles
  (map (lambda (role) (mkpiece 2 role))
       player-start-roles))

(define board-placement
  (shuffle (flatten (list player1-roles
                          player2-roles))))

(define (flip piece)
  (hash-set piece 'revealed #t))

(define board-locations
  (hash 0 (range 8)
        1 (range 8 16)
        2 (range 16 24)
        3 (range 24 32)))

(define (get-index-coordinates x y)
  (list-ref (hash-ref board-locations
                      y)
            x))

(define (piece-at-coordinates x y)
  (list-ref board-placement
            (get-index-coordinates x y)))
