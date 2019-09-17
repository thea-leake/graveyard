#lang racket/base
;; repl scratch pad - a few test maps, some repl session tidbits saved for referende later 
(require "banqi.rkt")
(require "cli-utils.rkt")

;; generated w/ (gen-board) from banqi.rkt
(define test-board '(#hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Cannon") (player . "Black") (empty . #f) (revealed . #t))
  #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Chariot") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Horse") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Chariot") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Cannon") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Advisor") (player . "Red") (empty . #f) (revealed . #t))
  #hash((role . "Advisor") (player . "Black") (empty . #f) (revealed . #t))
  #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Horse") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Cannon") (player . "Black") (empty . #f) (revealed . #t))
  #hash((role . "Horse") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Advisor") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Elephant") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Chariot") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Advisor") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Chariot") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Cannon") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Elephant") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Elephant") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "General") (player . "Black") (empty . #f) (revealed . #f))
  #hash((role . "Horse") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Elephant") (player . "Red") (empty . #f) (revealed . #f))
  #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))))


(define test-board-fakegame
  '(#hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "Horse") (player . "Black") (empty . #f) (revealed . #t))
    #hash((role . "Chariot") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "Cannon") (player . "Red") (empty . #f) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "Advisor") (player . "Red") (empty . #f) (revealed . #t))
    #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
    #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
    #hash((role . "Horse") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "Cannon") (player . "Black") (empty . #f) (revealed . #t))
    #hash((role . "Horse") (player . "Red") (empty . #f) (revealed . #f))
    #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
    #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #t))
    #hash((role . "Advisor") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "Elephant") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #t))
    #hash((role . "Soldier") (player . "Red") (empty . #f) (revealed . #f))
    #hash((role . "Advisor") (player . "Red") (empty . #f) (revealed . #t))
    #hash((role . "Chariot") (player . "Red") (empty . #f) (revealed . #f))
    #hash((role . "Cannon") (player . "Red") (empty . #f) (revealed . #t))
    #hash((role . "Elephant") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "General") (player . "Red") (empty . #f) (revealed . #f))
    #hash((role . "Horse") (player . "Red") (empty . #f) (revealed . #t))
    #hash((role . "General") (player . "Black") (empty . #f) (revealed . #t))
    #hash((role . "Elephant") (player . "Red") (empty . #f) (revealed . #f))
    #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))))

;; scratch.rkt> (is-valid-move? "Black" '(4 1) '(4 2) test-board-fakegame)
;; '(#f "Cannot capture a piece without jumping over another piece")
;; scratch.rkt> (is-valid-move? "Black" '(6 0) '(4 0) test-board-fakegame)
;; '(#f "Cannot move an opponents piece")
;; scratch.rkt> (is-valid-move? "Red" '(6 0) '(4 0) test-board-fakegame)
;; '(#t "Valid cannon move")
;; scratch.rkt> (display-board test-board-fakegame)
;; -------------------------------------------------------------------------------------------
;; || ___X____ | #Empty#_ | ___X____ | #Empty#_ | _Horse__ | ___X____ | _Cannon_ | #Empty#_ ||

;; || ___X____ | ________ | ___X____ | ________ | _Black__ | ___X____ | __Red___ | ________ ||
;; -------------------------------------------------------------------------------------------
;; || Advisor_ | ___X____ | ___X____ | ___X____ | _Cannon_ | ___X____ | ___X____ | ___X____ ||

;; || __Red___ | ___X____ | ___X____ | ___X____ | _Black__ | ___X____ | ___X____ | ___X____ ||
;; -------------------------------------------------------------------------------------------
;; || Soldier_ | ___X____ | ___X____ | #Empty#_ | Soldier_ | ___X____ | Advisor_ | ___X____ ||

;; || _Black__ | ___X____ | ___X____ | ________ | __Red___ | ___X____ | __Red___ | ___X____ ||
;; -------------------------------------------------------------------------------------------
;; || _Cannon_ | ___X____ | #Empty#_ | ___X____ | _Horse__ | General_ | ___X____ | ___X____ ||

;; || __Red___ | ___X____ | ________ | ___X____ | __Red___ | _Black__ | ___X____ | ___X____ ||
;; -------------------------------------------------------------------------------------------
;; scratch.rkt> (is-valid-move? "Red" '(0 1) '(0 2) test-board-fakegame)
;; '(#t "Valid move.")
;; scratch.rkt> (is-valid-move? "Red" '(0 2) '(0 1) test-board-fakegame)
;; '(#f "Cannot move an opponents piece")
;; scratch.rkt> (is-valid-move? "Black" '(0 2) '(0 1) test-board-fakegame)
;; '(#f "Target piece too powerful to capture")
;; scratch.rkt> (is-valid-move? "Black" '(0 2) '(0 3) test-board-fakegame)
;; '(#f "Target piece too powerful to capture")
;; scratch.rkt> 



(define test-board-endgame
  '(#hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "Elephant") (player . "Black") (empty . #f) (revealed . #f))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "#Empty#") (player . #\_) (empty . #t) (revealed . #t))
    #hash((role . "Soldier") (player . "Black") (empty . #f) (revealed . #f))))
