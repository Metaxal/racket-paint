#lang racket/base

(require "paint.rkt"
         racket/class
         racket/dict
         global)

(void (globals->command-line))

(define widget-dict (create-frame))

(load-keymap) ; load user-defined keybindings

(define frame (dict-ref widget-dict 'frame))
(send frame show #t)
