;; Copyright 2019-2021 Thea Leake

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
