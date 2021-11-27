#lang racket
(require "keymap.rkt"
         "misc.rkt"
         (except-in racket/gui/base keymap%)
         search-list-box)

(provide (all-defined-out))

(define (get-mouse-state)
  (define-values (pt mods) (get-current-mouse-state))
  (list* (send pt get-x) (send pt get-y) mods))

(define (system-info)
  (for/list ([x '(os os* arch word vm gc link machine target-machine
                     so-suffix so-mode fs-change cross)])
    (cons x (system-type x))))

(define meditor-canvas%
  (class editor-canvas%
    (init-field [full-event? #false] [erase? #true] [mouse-state? #false])
    (define filtered '(motion enter leave shift rshift control rcontrol release))

    (define/public (filter-type/key ev-type yes?)
      (set! filtered (remq ev-type filtered))
      (unless yes?
        (set! filtered (cons ev-type filtered))))

    (define/public (filter-type/key? ev-type)
      (memq ev-type filtered))

    (define last-event-dict #f)
    (define/public (get-last-event-dict) last-event-dict)
    
    (define/override (on-subwindow-char receiver ev)
      (handle-event receiver ev))
    
    (define/override (on-subwindow-event receiver ev)
      (handle-event receiver ev))

    (define/public (handle-event receiver ev)
      (define ev-dict (event->dict ev))
      (unless (memq
               (or (dict-ref ev-dict 'event-type #f)
                   (dict-ref ev-dict 'key-code '()))
               filtered)
        (set! last-event-dict ev-dict)
        (define simple-ev-dict (simplify-event-dict ev-dict))
        
        (define str
          (string-append
           (simplified-event-dict->string simple-ev-dict)
           "\n\n"
           (pretty-format (if full-event? ev-dict simple-ev-dict)
                          80
                          #:mode 'write)
           (if mouse-state?
             (string-append "\n" (pretty-format (get-mouse-state) 80 #:mode 'write) "\n")
             "\n")))
        (define text (send this get-editor))
        (if erase? (send text erase) (send text insert "\n"))
        (send text insert str)))
    
    (super-new)))

(define (show-event-listener-dialog #:parent [parent #f]
                                    #:message [message #f]
                                    #:full-event? [full-event? #false]
                                    #:clipboard-button? [clipboard-button? #false]
                                    #:erase? [erase? #true]
                                    #:mouse-state? [mouse-state? #false]
                                    #:system-info? [system-info? #false])

  (define last-ev #f)
  
  (define fr (new dialog% [label "Get event"] [parent parent]
                  [width 500] [height 600]))
  (when message
    (void (new message% [parent fr] [label message])))

  (define cv (new meditor-canvas% [parent fr] [editor (new text%)]
                  [erase? erase?]
                  [mouse-state? mouse-state?]))
  (set-field! full-event? cv full-event?)

  (define cbx-panel (new horizontal-panel% [parent fr]
                         [alignment '(center center)]
                         [stretchable-height #f]))
  (for ([type (in-list '(motion enter leave shift rshift control rcontrol release))])
    (new check-box% [parent cbx-panel]
         [label (format "~a" type)]
         [value (not (send cv filter-type/key? type))]
         [callback (λ (cb ev)
                     (send cv filter-type/key type (send cb get-value))
                     (send cv focus))]))

  (define bt2-panel (new horizontal-panel% [parent fr]
                         [alignment '(center center)]
                         [stretchable-height #f]))

  (void (new check-box% [parent bt2-panel]
             [label "full event"]
             [value (get-field full-event? cv)]
             [callback (λ (cb ev)
                         (set-field! full-event? cv (send cb get-value))
                         (send cv focus))]))
  (void (new check-box% [parent bt2-panel]
             [label "mouse state"]
             [value (get-field mouse-state? cv)]
             [callback (λ (cb ev)
                         (set-field! mouse-state? cv (send cb get-value))
                         (send cv focus))]))
  
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
  (define bt-clip
    (and clipboard-button?
         (new button% [parent ok-cancel-panel] [label "Copy"]
              [callback (λ _
                          (send the-clipboard set-clipboard-string
                                (send (send cv get-editor) get-text)
                                0)
                          ; Give the focus back to the canvas.
                          (send cv focus))])))

  (when system-info?
    (send (send cv get-editor) insert
          (string-append (pretty-format (system-info) 80) "\n")))

  (send cv focus)
  (send fr show #t)
  last-ev)

(define (keymap-shortcuts/frame keymap
                                #:parent [parent #f])
  (new search-list-box-frame% [parent parent] [label "Shortcuts"]
               [contents
                (sort (hash->list (send keymap get-mappings))
                      string<=? #:key cdr)]
               [key (λ (content)
                      (string-append (simplified-event-dict->string
                                      (simplify-event-dict (car content)))
                                     "\t"
                                     (cdr content)))]
               [callback (λ (idx lbl content)
                           (send keymap call-function (cdr content) #f (new key-event%)))]))

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
                (string-append (simplified-event-dict->string
                                (simplify-event-dict (car content)))
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
  (show-event-listener-dialog #:full-event? #true #:clipboard-button? #true))
(module+ main
  (require global)
  (define-global:boolean *clip-button?* #true "Show the clipboard button")
  (define-global:boolean *full-event?* #true "Full event by default")
  (define-global:boolean *erase?* #true "Erase the canvas after each event")
  (define-global:boolean *mouse-state?* #false
    "Also include the mouse state as reported by (get-current-mouse-state)")
  (define-global:boolean *system-info?* #false "Include system information")
  (void (globals->command-line))
  (show-event-listener-dialog #:full-event? (*full-event?*)
                              #:clipboard-button? (*clip-button?*)
                              #:erase? (*erase?*)
                              #:mouse-state? (*mouse-state?*)
                              #:system-info? (*system-info?*)))

