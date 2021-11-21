#lang racket
(require "keymap.rkt"
         "misc.rkt"
         (except-in racket/gui/base keymap%)
         search-list-box)

(provide (all-defined-out))

(define mcanvas%
  (class canvas%
    (init-field [event-filter values])

    (define last-event-dict #f)
    (define/public (get-last-event-dict) last-event-dict)
    
    (define/override (on-subwindow-char receiver ev)
      (handle-event receiver ev))
    
    (define/override (on-subwindow-event receiver ev)
      (handle-event receiver ev))

    (define/override (on-scroll ev)
      (handle-event this ev))

    (define/public (handle-event receiver ev)
      (define ev-dict (event->dict ev))
      (when (event-filter ev-dict)
        (set! last-event-dict ev-dict)
        #;(pretty-write (simplify-event-dict ev-dict))
        (define str (pretty-format (simplify-event-dict ev-dict)
                             80
                             #:mode 'write))
        (define lines (string-split str "\n"))
        (define dc (send this get-dc))
        (send dc clear)
        (define line-height (send (send dc get-font) get-size))
        (for ([line (in-list lines)]
              [i (in-naturals)])
          (send dc draw-text line 0 (* i (+ 2 line-height))))))
    
    (super-new)))

(define (show-event-listener-dialog #:parent [parent #f])

  (define last-ev #f)
  
  (define fr (new dialog% [label "Get event"] [parent parent]
                  [width 500] [height 400]))

  (define cv (new mcanvas% [parent fr]
                  [event-filter
                   (λ (ev-dict)
                     (or
                      #;#true
                      (and (not (memq (dict-ref ev-dict 'event-type #f)
                                      '(motion enter leave)))
                           (not (memq (dict-ref ev-dict 'key-code #f)
                                      '(shift rshift control rcontrol release)))))
                     #;(eq? 'left-down (dict-ref data 'event-type #f)))]))

  (define ok-cancel-panel (new horizontal-panel% [parent fr]
                               [alignment '(center center)]
                               [stretchable-height #f]))
  (define bt-ok (new button% [parent ok-cancel-panel] [label "Ok"]
                     [callback (λ (bt ev)
                                 (set! last-ev (send cv get-last-event-dict))
                                 (send fr show #f))]))

  (define bt-cancel (new button% [parent ok-cancel-panel] [label "Cancel"]
                         [callback (λ (bt ev)
                                     (set! last-ev #false)
                                     (send fr show #f))]))

  (send cv focus)
  (send fr show #t)
  last-ev)

;; Another tool to map an existing function to a new event
(define (keymap-map-function/frame keymap
                                   #:parent [parent #f]
                                   #:callback [callback (λ (keymap name ev) (void))])
  (define slbf
    (new search-list-box-frame% [parent parent]
         [label "New shortcut"]
         [message "Choose a function to map"]
         [contents (sort (hash-keys (send keymap get-functions)) string<?)]
         [callback (λ (idx lbl name)
                     (define ev (show-event-listener-dialog))
                     (when ev
                       (send keymap map-function name ev)
                       (send slbf show #false)
                       (callback keymap name ev)))]))
  slbf)

(define (keymap-remove-mapping/frame keymap
                                     #:parent [parent #f]
                                     #:callback [callback (λ (keymap name event-dict) (void))])
  (define slbf
    (new search-list-box-frame% [parent parent]
         [label "Remove shortcut"]
         [message "Choose a shortcut to remove"]
         [contents
          (sort (hash->list (send keymap get-mappings))
                string<=? #:key cdr)]
         [key (λ (content)
                (string-append (~a (simplify-event-dict (car content)))
                               "\t"
                               (cdr content)))]
         [callback (λ (idx lbl content)
                     (define ev-dict (car content))
                     (define name (cdr content))
                     (send keymap remove-mapping ev-dict)
                     (callback keymap name ev-dict)
                     (send slbf show #false))])) ; refreshing would be better…
  slbf)

(module+ drracket
  (show-event-listener-dialog))

