#lang racket/base

(require "misc.rkt"
         racket/dict
         racket/contract
         racket/class
         racket/list
         racket/format
         racket/string
         racket/gui/base)


;;; Why a new keymap% class:
;;; - mostly because default class often fails due to trying to be smart in many cases.
;;;   Instead, the new keymap may be stupid, but is much easier to customize thanks to
;;;   the event listener that will obtain the exact key combination intended to call
;;;   a function.
;;; - Handcrafting a correct mouse-event% is tricky, and varies from plateform to plateform
;;;   See: https://github.com/racket/gui/issues/252#issuecomment-976522484
;;; - easy to check equality between events
;;; - serializable events
;;; -


;;; TODO:
;;; - unmap all shortcuts for a function name
;;; - unmap a particular shortcut
;;; - save to disk, restore from disk and/or save in some preferences
;;; - event-dict->string to display in a short string
;;; - maybe we should get rid of the `other-...` when testing for equality?
;;; - Chain keymaps

(provide (all-defined-out))


(define (when-cons test v lst)
  (if test (cons v lst) lst))

(define-syntax-rule (for-all-buttons ev method)
  (when-cons
   (send ev method 'left)
   'left
   (when-cons
    (send ev method 'middle)
    'middle
    (when-cons
     (send ev method 'right)
     'right
     '()))))

;==============;
;=== Keymap ===;
;==============;

;;; event-dicts are serializable association lists.

(define (event-dict? x)
  (and (dict? x)
       (dict-has-key? x 'event-class)))

(define (key-event->dict ev)
  (check-argument ev (is-a?/c key-event%))
  `((event-class                . key-event%) ; symbol, not actual class, to be serializable
    (key-code                   . ,(send ev get-key-code))
    (key-release-code           . ,(send ev get-key-release-code))
    (alt-down                   . ,(send ev get-alt-down))
    (caps-down                  . ,(send ev get-caps-down))
    (control-down               . ,(send ev get-control-down))
    (control+meta-is-altgr      . ,(send ev get-control+meta-is-altgr))
    (shift-down                 . ,(send ev get-shift-down))
    (meta-down                  . ,(send ev get-meta-down))
    (mod3-down                  . ,(send ev get-mod3-down))
    (mod4-down                  . ,(send ev get-mod4-down))
    (mod5-down                  . ,(send ev get-mod5-down))
    (other-altgr-key-code       . ,(send ev get-other-altgr-key-code))
    (other-caps-key-code        . ,(send ev get-other-caps-key-code))
    (other-shift-altgr-key-code . ,(send ev get-other-shift-altgr-key-code))
    (other-shift-key-code       . ,(send ev get-other-shift-key-code))
    (wheel-steps                . ,(send ev get-wheel-steps))
    (x                          . ,(send ev get-x))
    (y                          . ,(send ev get-y))))

(define (mouse-event->dict ev)
  (check-argument ev (is-a?/c mouse-event%))
  `((event-class     . mouse-event%) ; symbol, not actual class, to be serializable
    (event-type      . ,(send ev get-event-type))
    (buttons-changed . ,(for-all-buttons ev button-changed?))
    #;(buttons-down    . ,(for-all-buttons ev button-down?)) ; unreliable
    #;(buttons-up      . ,(for-all-buttons ev button-up?))
    (left-down       . ,(send ev get-left-down))
    (middle-down     . ,(send ev get-middle-down))
    (right-down      . ,(send ev get-right-down))
    (dragging        . ,(send ev dragging?))
    (entering        . ,(send ev entering?))
    (leaving         . ,(send ev leaving?))
    (moving          . ,(send ev moving?))
    (alt-down        . ,(send ev get-alt-down))
    (caps-down       . ,(send ev get-caps-down))
    (control-down    . ,(send ev get-control-down))
    (shift-down      . ,(send ev get-shift-down))
    (meta-down       . ,(send ev get-meta-down))
    (mod3-down       . ,(send ev get-mod3-down))
    (mod4-down       . ,(send ev get-mod4-down))
    (mod5-down       . ,(send ev get-mod5-down))
    (x               . ,(send ev get-x))
    (y               . ,(send ev get-y))))

(define (scroll-event->dict ev)
  (check-argument ev (is-a?/c scroll-event%))
  `((event-class . scroll-event)
    (event-type . ,(send ev get-event-type))
    (direction . ,(send ev get-direction))
    (position . ,(send ev get-position))))

(define (event->dict ev)
  (cond [(is-a? ev key-event%)
         (key-event->dict ev)]
        [(is-a? ev mouse-event%)
         (mouse-event->dict ev)]
        [(is-a? ev scroll-event%)
         (scroll-event->dict ev)]
        [else
         (raise-argument-error 'ev "one of: key-event% mouse-event% scroll-event%" ev)]))

;; The value of these events should not matter when checking a mapped event
(define keymap-filtered-keys
  '(other-altgr-key-code
    other-caps-key-code
    other-shift-altgr-key-code
    other-shift-key-code
    wheel-steps
    x
    y))

(define (simplify-event-dict ev-dict)
  (if (dict-has-key? ev-dict 'properties)
    ev-dict ; already simplified
    (cons
     (cons 'properties (filter-map (λ (p) (and (eq? #t (cdr p)) (car p))) ev-dict))
     (filter-not (λ (p) (or (boolean? (cdr p))
                            (memq (car p) keymap-filtered-keys)))
                 ev-dict))))

(define (simplified-event-dict->string ev-dict)
  (define props
    (string-join (map
                  (λ (s) (string-replace (~a s) "-down" ""))
                  (dict-ref ev-dict 'properties '())) ":"))
  (unless (equal? props "")
    (set! props (string-append props ":")))
  (case (dict-ref ev-dict 'event-class)
    [(key-event%)
     (string-append
      props
      (or (~a (dict-ref ev-dict 'key-code #f))
          (~a (dict-ref ev-dict 'key-release-code #f))))]
    [(mouse-event%)
     (string-append
      props
      (~a (dict-ref ev-dict 'event-type)))]
    [else (format "~v" ev-dict)])
  )

(define keymap%
  (class object%

    (init-field [parent #f]) ; the parent keymap
    (check-argument parent (or/c #false (is-a?/c keymap%)))
    
    (define functions (make-hash))
    (define mappings (make-hash))

    ;; Turns an event% or an event-dict into an event-dict suitable for a keymap
    (define (event->keymap-event ev)
      (simplify-event-dict
       (if (is-a? ev event%)
         (event->dict ev)
         ev)))

    (define/public (get-functions) functions)
    (define/public (get-mappings) mappings)

    ; Erases all mappings, but not the added functions
    (define/public (clear-mappings)
      (set! mappings (make-hash)))

    ;; proc: receiver event-dict -> any
    ;; Fails if the name is already mapped to a function.
    (define/public (add-function name proc . events)
      (check-argument name string?)
      (check-argument proc (procedure-arity-includes/c 2))
      (when (hash-has-key? functions name)
        (error "function already mappend in keymap" name proc))
      (hash-set! functions name proc)
      (for ([ev (in-list events)])
        (map-function name ev)))

    (define/public (function-exists? name)
      (check-argument name string?)
      (hash-has-key? functions name))

    (define/public (get-function name) ; can be a substitute for function-exists?
      (check-argument name string?)
      (hash-ref functions name #f))

    (define/public (get-mapping ev)
      (check-argument ev (or/c (is-a?/c event%) event-dict?))
      (hash-ref mappings (event->keymap-event ev) #f))

    (define/public (remove-function name)
      (hash-remove! functions name))

    (define/public (remove-function-mappings name)
      (for ([(ev fname) (in-dict mappings)])
        (when (equal? fname name)
          (hash-remove! mappings ev))))

    (define/public (remove-mapping ev)
      (check-argument ev (or/c (is-a?/c event%) event-dict?))
      (hash-remove! mappings (event->keymap-event ev)))

    ;; `name` may not be mapped yet to a function.
    ;; This allows defining (or loading from disk) a keymap
    ;; before defining the functions and adding them.
    (define/public (map-function name . events)
      (check-argument name string?)
      (for ([ev (in-list events)])
        (check-argument ev (or/c (is-a?/c event%) event-dict?))
        (hash-set! mappings (event->keymap-event ev) name)))

    (define/public (call-function name receiver ev)
      (check-argument ev (is-a?/c event%))
      (check-argument name string?)
      (or
       (let ([ev-dict (event->keymap-event ev)])
         (define fun (and name (hash-ref functions name #f)))
         (and fun (fun receiver ev))) ; we use the original event%
       ; no dice, let's try the parent keymap
       (and parent (send parent call-function receiver ev))))

    ;; Returns #f if the event is not mapped to a function.
    (define/public (handle-event receiver ev)
      (check-argument ev (is-a?/c event%))
      (or
       (let ([ev-dict (event->keymap-event ev)])
        (define name (hash-ref mappings ev-dict #f))
        (define fun (and name (hash-ref functions name #f)))
        (and fun (fun receiver ev))) ; we use the original event%
       ; no dice, let's try the parent keymap
       (and parent (send parent handle-event receiver ev))))
    
    (super-new)))

(module+ test
  (require rackunit)
  (let ()
    (define km (new keymap%))
    (define test-passed! "test-passed!")
    (send km add-function "test" (λ (receiver ev-dict)
                                   test-passed!))
    (send km map-function "test"
          (event->dict (new key-event% [key-code #\a]))
          (event->dict (new key-event% [key-code #\b] [control-down #t]))
          (event->dict (new mouse-event% [event-type 'right-down])))

    (check-equal? (send km handle-event #f (new key-event% [key-code #\a]))
                  test-passed!)
    (check-equal? (send km handle-event #f (new key-event% [key-code #\b] [control-down #t]))
                  test-passed!)
    (check-equal? (send km handle-event #f (new mouse-event% [event-type 'right-down]))
                  test-passed!)
    (check-equal? (send km handle-event #f (new key-event% [key-code #\b]))
                  #f)
    (check-equal? (send km handle-event #f (new mouse-event% [event-type 'left-down]))
                  #f)
    (check-equal? (send km handle-event #f (new mouse-event%
                                                [event-type 'right-down]
                                                [control-down #t]))
                  #f)
    
    ))
