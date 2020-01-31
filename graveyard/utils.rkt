#lang racket/base

(require (only-in racket/string
                  string-join)
         (only-in racket/format
                  ~a))
(provide inspect)

(define (inspect expression #:header [header "## Inspecting"])
  (display (string-join (list header
                              (~a expression))
                        ": "
                        #:before-first "\n"
                        #:after-last "\n"))
  expression)
