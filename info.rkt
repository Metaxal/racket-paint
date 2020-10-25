#lang info
(define collection "racket-paint")
(define deps '("gui-lib"
               "pict-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/racket-paint.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(laurent))
(define test-omit-paths '("main.rkt"))