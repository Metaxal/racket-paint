#lang racket/base

(require "misc.rkt"
         "keymap.rkt"
         "event-listener.rkt"
         (except-in racket/gui/base keymap%)
         pict
         search-list-box
         racket/contract
         racket/class
         racket/match
         racket/list
         racket/dict
         racket/file
         racket/format)

(provide (all-defined-out))

(define line-width-init 1)
(define line-width-min 1)
(define line-width-max 100)

(define rktp-pref-dir (build-path (find-system-path 'pref-dir) "racket-paint"))
(define keymap-file (build-path rktp-pref-dir "keymap.rktd"))

(define (save-keymap)
  (make-directory* rktp-pref-dir)
  (write-to-file
   (hash->list (send canvas-keymap get-mappings))
   keymap-file
   #:exists 'replace))

(define (load-keymap)
  (when (file-exists? keymap-file)
    (send canvas-keymap clear-mappings)
    (define d (file->value keymap-file))
    (for ([(ev-dict name) (in-dict d)])
      (send canvas-keymap map-function name ev-dict))))

;; TODO:
;; - draw rectangle, filled-rectangle
;; - change background color

(define canvas-keymap (new keymap%))
(define button-keymap (new keymap%))



(define keymapped<%>
  (interface ()
    get-keymap))

;; Makes the window<%> listen to events and forward them to the keymap.
;; This applies to most GUI widgets, except menus and menu-items.
(define keymapped-mixin
  (mixin (window<%>) (keymapped<%>)
    (init-field keymap)
    
    (define/public (get-keymap) keymap)
    
    (define/override (on-subwindow-char receiver ev) ; better than `on-char`?
      (or (send keymap handle-event receiver ev)
          (super on-subwindow-char receiver ev)))
    
    (define/override (on-subwindow-event receiver ev) ; better than `on-event`?
      (or (send keymap handle-event receiver ev)
          (super on-subwindow-event receiver ev)))
      
    (super-new)))

;; Applies to widgets that have a callback init argument.
;; If the callback-name is already associated to a function in the keymap,
;; then the callback must be #f or eq? to that function, otherwise an exception is raised.
;; Otherwise, the callback is added as a function to the keymap, associated with the
;; callback-name.
;; 
;; We could also simply search the menus for the item callbacks, but then we couldn't
;; share callbacks.
(define keymapped-callback<%>
  (interface ()
    get-callback-keymap ; may not be the same as for keymapped<%>
    get-callback-name))

(define keymapped-callback-mixin
  (mixin () (keymapped-callback<%>)
    (init callback)
    (init-field callback-name callback-keymap) ; may not be the same as label, and label may not be text
    (check-argument callback-name string?)

    (define/public (get-callback-keymap) callback-keymap)
    (define/public (get-callback-name) callback-name)    

    ; defines the callback if keymap-name already exists
    (let ([proc (send callback-keymap get-function callback-name)])
      (if proc
        (if callback
          (unless (eq? callback proc)
            (error "Unused callback: using mapped function" callback-name))
          (set! callback proc))
        (if (procedure? callback)
          (send callback-keymap add-function callback-name callback)
          (error "Callback must be a procedure" callback))))
    
    (super-new [callback callback])))

(define my-canvas%
  (class canvas%
    (define color "black")
    (define line-width line-width-init)
    (define commands '())
    (define tool 'freehand) ; freehand rectangle filled-rectangle

    (init-field [on-set-line-width (λ (cv width) (void))]
                [background-color "white"])
    
    (define/override (on-event ev)
      (when (send ev get-left-down)
        (when (send ev button-changed? 'left)
          (new-line))
        (define x (send ev get-x))
        (define y (send ev get-y))
        (add-point x y)))

    (define/public (get-commands)
      commands)

    (define/public (clear-commands)
      (set! commands '())
      (set-color color)
      (set-line-width line-width)
      (send this refresh))

    (define/public (set-color c)
      (set! color
            (if (is-a? c color%)
              (list (send c red) (send c green) (send c blue))
              c))
      (set! commands
        (cons (list 'color color)
              (match commands
                [`((color ,c-old) . ,rst) rst] ; replace
                [else commands])))
      (do-last-command))

    (define/public (new-line)
      ; start a new line
      (match commands
        [(list-rest '(points) _rst) ; already a new line
         (void)]
        [else (set! commands (cons '(points) commands))]))

    (define/public (get-last-point)
      (match commands
        [(list-rest (list-rest 'points (cons x y) _rst-pts) _rst-cmds)
         (cons x y)]
        [else #false]))
    
    (define/public (add-point x y)
      (define pos (cons x y))
      (define-values (prev-points rest-commands)
        (match commands
          [(list-rest (list-rest 'points prev-pts) rst) ;  add to previous list of points
           (values prev-pts rst)]
          [else (values '() commands)]))
      (define x0.y0 (and (not (empty? prev-points)) (first prev-points)))
      (set! commands
            (cons (list* 'points pos prev-points)
                  rest-commands))
      ; Draw directly to avoid having to redraw everything all the time
      (when x0.y0
        (send (send this get-dc) draw-line (car x0.y0) (cdr x0.y0) x y)))

    (define/public (get-line-width) line-width)

    (define/public (set-line-width w)
      (set! line-width w)
      (set! commands
        (cons (list 'line-width w) 
              (match commands
                [`((line-width ,w-old) . ,rst) rst] ; replace
                [else commands])))
      (do-last-command)
      (on-set-line-width this w))

    (define/public (set-tool t)
      (set! tool t))

    (define/public (undo-command)
      (unless (empty? commands)
        (set! commands (rest commands))
        (send this refresh)))

    (define/public (save-file f)
      (write-to-file commands f #:exists 'replace))
    
    (define/public (open-file f)
      (when (file-exists? f)
        (set! commands (with-input-from-file f read))
        (send this refresh)))

    (define/public (do-command cmd [dc (send this get-dc)])
      (match cmd
        [`(line-width ,w)
         (define p (send dc get-pen))
         (send dc set-pen (send p get-color) w 'solid)]
        [`(color ,c)
         (define p (send dc get-pen))
         (send dc set-pen (if (list? c) (apply make-color c) c)
               (send p get-width) 'solid)]
        [`(points . ,pts)
         (send dc draw-lines pts)]
        [else (error "Unknown command: " cmd)]))

    (define/public (do-last-command [dc (send this get-dc)])
      (match (get-commands)
        [(list-rest cmd _rst-cmds)
         (do-command cmd dc)]
        [else (void)]))

    (define/public (draw dc)
      (define commands (reverse (get-commands)))
      (send dc set-background background-color)
      (send dc clear)
      ; Not efficient to redraw all the lines each time. We should keep the previous
      ; picture and draw on top of it instead.
      (for ([cmd (in-list commands)])
        (do-command cmd dc)))

    
    (begin
      (send canvas-keymap add-function "increase-brush-size"
            (λ _ (set-line-width (min line-width-max (+ line-width 1))))
            (new key-event% [key-code #\+] [control-down #true])
            (new key-event% [key-code #\x])
            (new key-event% [key-code 'wheel-up]))
      (send canvas-keymap add-function "decrease-brush-size"
            (λ _ (set-line-width (max line-width-min (- line-width 1))))
            (new key-event% [key-code #\-] [control-down #true])
            (new key-event% [key-code #\y])
            (new key-event% [key-code 'wheel-down])))
    
    (super-new)
    (send (send this get-dc) set-smoothing 'aligned)
    (clear-commands)))

(define fr (new frame% #;(keymapped%% frame%)
                #;[keymap keymap]
                [label "Racket Paint"]
                [width 500] [height 500]))

(define bt-panel (new horizontal-panel% [parent fr] [stretchable-height #f]))

(define (make-button-color-label color)
  (pict->bitmap (colorize (filled-rectangle 20 20) color)))

;; Change the keymap mappings for the callback of a selected widget,
;; if it is a keymapped-callback-widget<%>.
(send button-keymap add-function
      "change-color-button-callback-mapping"
      (λ (bt bt-ev)
        (when (is-a? bt keymapped-callback<%>)
          (define ev (show-event-listener-dialog #:parent (send bt get-top-level-window)))
          (when ev
            (define keymap (send bt get-callback-keymap))
            (define callback-name (send bt get-callback-name))
            (send keymap remove-function-mappings callback-name) ; remove all old shortcuts
            (send keymap map-function callback-name ev)
            (save-keymap))))
      (new mouse-event% [event-type 'left-down] [control-down #true]))

(define color-button%
  (class button% 
    (init-field get-canvas
                [color (send the-color-database find-color "black")])
    (define/override (on-subwindow-event bt ev)
      (or
       (super on-subwindow-event bt ev) ; let the parent process first
       (case (send ev get-event-type)
        [(left-up)
         (send (get-canvas) set-color color)
         #f] ; #f just to get the button press feel
        [(right-up)
         (define c (get-color-from-user #f #f color))
         (when c
           (set! color c)
           (send (get-canvas) set-color c)
           (send bt set-label (make-button-color-label c))
           (send bt refresh))
         #f]
         [else #t])))
    (super-new
     [horiz-margin 0] [vert-margin 0]
     [label (make-button-color-label color)])))

(define color-buttons
  (for/list ([color '("black" "white" "red" "green" "blue")] ; initial colors, may be changed
             [i (in-naturals 1)])
    (define name (format "color ~a" i)) ; not the label
    (define bt
      (new (keymapped-mixin (keymapped-callback-mixin color-button%))
           [keymap button-keymap]
           [callback-keymap canvas-keymap]
           [callback-name name]
           [parent bt-panel]
           [color (send the-color-database find-color color)]
           [get-canvas (λ () cv)]
           [callback (λ _ (send cv set-color (get-field color bt)))]))
    bt))

(define bt-erase (new (keymapped-mixin (keymapped-callback-mixin button%))
                      [parent bt-panel]
                      [label "Clear"]
                      [keymap button-keymap]
                      [callback-keymap canvas-keymap]
                      [callback-name "clear"]
                      [callback (λ (bt ev) (send cv clear-commands))]))

(define bt-undo (new (keymapped-mixin (keymapped-callback-mixin button%))
                     [parent bt-panel]
                     [label "Undo"]
                     [keymap button-keymap]
                     [callback-keymap canvas-keymap]
                     [callback-name "undo"]
                     [callback (λ (bt ev) (send cv undo-command))]))

(void (new grow-box-spacer-pane% [parent bt-panel]))

(define menu-bar (new menu-bar% [parent fr]))
(define file-menu (new menu% [parent menu-bar] [label "&File"]))

;; TODO: Should we have a `menu-keymap` that holds all the callbacks of the menus?

(send canvas-keymap add-function "open"
      (λ (receiver ev)
        (define f
          (get-file "Open a file" fr #f #f "rktp" '()
                    '(("Racket Paint" "*.rktp") ("Any" "*.*"))))
        (when f (send cv open-file f)))
      (new key-event% [key-code #\o] [control-down #true]))

(send canvas-keymap add-function "save"
      (λ (receiver ev)
        (define f
          (put-file "Save file" fr #f #f "rktp" '()
                    '(("Racket Paint" "*.rktp") ("Any" "*.*"))))
        (when f (send cv save-file f)))
      (new key-event% [key-code #\s] [control-down #true]))

(define file:open-menu-item
  (new menu-item% [parent file-menu] [label "Open"]
       [callback (send canvas-keymap get-function "open")]))

(define file:save-menu-item
  (new menu-item% [parent file-menu] [label "Save"]
       [callback (send canvas-keymap get-function "save")]))


(define keymap-menu (new menu% [parent menu-bar] [label "Keymaps"]))

(define (make-keymap-menu kmp label #:parent parent-menu)
  (define submenu (new menu% [parent parent-menu] [label label]))
  (list
   submenu
   (new menu-item% [parent submenu] [label "Shortcuts"]
        [callback
         (λ (bt ev) (keymap-shortcuts/frame kmp #:parent fr))])
  
   (new menu-item% [parent submenu] [label "New shortcut"]
        [callback
         (λ _
           (keymap-map-function/frame kmp
                                      #:parent fr
                                      #:callback
                                      (λ (keymap name ev)
                                        (when ev (save-keymap)))))])
  
   (new menu-item% [parent submenu] [label "Remove shortcut"]
        [callback
         (λ _
           (keymap-remove-mapping/frame kmp
                                        #:parent fr
                                        #:callback
                                        (λ (keymap name ev)
                                          (when ev (save-keymap)))))])))

(make-keymap-menu canvas-keymap "Canvas" #:parent keymap-menu)
(make-keymap-menu button-keymap "Buttons" #:parent keymap-menu)


(define width-slider (new slider% [parent fr] [label "Line width"]
                          [min-value line-width-min]
                          [max-value line-width-max]
                          [init-value line-width-init]
                          [callback (λ (sl ev)
                                      (send cv set-line-width (send sl get-value)))]))

(define cv (new (keymapped-mixin my-canvas%) [parent fr]
                [keymap canvas-keymap]
                [on-set-line-width
                 (λ (cv width)
                   (send width-slider set-value width)
                   (send width-slider refresh))]
                [paint-callback
                 (λ (cv dc)
                   (send cv draw dc)
                   (send width-slider set-value (send cv get-line-width))
                   (send width-slider refresh))]))

(load-keymap) ; in case one already exists

(module+ drracket  
  (send fr show #t))
