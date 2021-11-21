#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out))


;; if `check` is a s-expr and it does not evaluate to a procedure of arity 1,
;; then it is considered an expression evaluating to the value of the test.
;; Otherwise, it is considered a procedure of arity 1 that
;; must be applied to `arg` to obtain the value of the test (this includes most
;; contrats).
(define-syntax (check-argument stx)
  (syntax-parse stx
    #:context stx
    #:track-literals
    [(_ arg:id check:id)
     #'(unless (check arg)
         (raise-argument-error 'arg (format "~a" 'check) arg))]
    [(_ arg:id check)
     #'(let ([check-val check])
         (if (and (procedure? check-val)
                  (procedure-arity-includes? check-val 1))
           (unless (check-val arg)
             (raise-argument-error 'arg (format "~a" 'check) arg))
           (unless check-val
             (raise-argument-error 'arg (format "~a" 'check) arg))))]))

