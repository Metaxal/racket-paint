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
   (hash->list (send keymap get-mappings))
   keymap-file
   #:exists 'replace))

(define (load-keymap)
  (when (file-exists? keymap-file)
    (send keymap clear-mappings)
    (define d (file->value keymap-file))
    (for ([(ev-dict name) (in-dict d)])
      (send keymap map-function name ev-dict))))

;; TODO:
;; - draw rectangle, filled-rectangle
;; - change background color

(define keymap (new keymap%))

;; TODO: This should be a mixin with an interface for easy checking.
;; TODO: Export this to keymap.rkt or some othe file
(define (keymapped%% wnd-class keymap)
  (check-argument wnd-class (implementation?/c window<%>))
  (check-argument keymap (is-a?/c keymap%))
  (cond
    [(is-a? wnd-class canvas%)
     (class wnd-class
       (define/override (on-subwindow-char receiver ev) ; better than `on-char`?
         (or (send keymap handle-event receiver ev)
             (super on-subwindow-char receiver ev)))
    
       (define/override (on-subwindow-event receiver ev) ; better than `on-event`?
         (or (send keymap handle-event receiver ev)
             (super on-subwindow-event receiver ev)))

       (define/override (on-scroll ev)
         (or (send keymap handle-event this ev)
             (super on-scroll ev)))
      
       (super-new))]
    [else
     (class wnd-class
       (define/override (on-subwindow-char receiver ev)
         (or (send keymap handle-event receiver ev)
             (super on-subwindow-char receiver ev)))
    
       (define/override (on-subwindow-event receiver ev)
         (or (send keymap handle-event receiver ev)
             (super on-subwindow-event receiver ev)))

       (super-new))]))

(define my-canvas%
  (class (keymapped%% canvas% keymap)
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
      (send keymap add-function "increase-brush-size"
            (λ _ (set-line-width (min line-width-max (+ line-width 1))))
            (new key-event% [key-code #\+] [control-down #true])
            (new key-event% [key-code #\x])
            (new key-event% [key-code 'wheelup]))
      (send keymap add-function "decrease-brush-size"
            (λ _ (set-line-width (max line-width-min (- line-width 1))))
            (new key-event% [key-code #\-] [control-down #true])
            (new key-event% [key-code #\y])
            (new key-event% [key-code 'wheeldown])))
    
    (super-new)
    (send (send this get-dc) set-smoothing 'aligned)
    (clear-commands)))

(define fr (new (keymapped%% frame% keymap)
                [label "Racket Paint"]
                [width 500] [height 500]))

(define bt-panel (new horizontal-panel% [parent fr] [stretchable-height #f]))

(define (make-button-color-label color)
  (pict->bitmap (colorize (filled-rectangle 20 20) color)))

(define keymapped-button%
  (class button%
    (init callback)
    (init-field keymap-name) ; may not be the same as label, and label may not be text
    (check-argument keymap-name string?)

    (send keymap add-function keymap-name callback)
    
    (define/override (on-subwindow-event bt bt-ev)
      (cond
        [(and (eq? (send bt-ev get-event-type)
                   'left-up)
              (send bt-ev get-control-down))
         (define ev (show-event-listener-dialog #:parent (send this get-top-level-window)))
         (when ev
           (send keymap remove-function-mappings keymap-name) ; remove old shortcut
           (send keymap map-function keymap-name ev)
           (save-keymap))]
        [else (super on-subwindow-event bt bt-ev)]))
    
    (super-new [callback callback])))

;; TODO: Make the ctrl-click to change the shortcut a mixin!
;;  and use that for all buttons and more!
(define color-button%
  (class keymapped-button%
    (init-field get-canvas
                [color (send the-color-database find-color "black")])
    (define/override (on-subwindow-event bt ev)
      (or
       (super on-subwindow-event bt ev)
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
     [label (make-button-color-label color)])))

(define init-buttons
  (for/list ([color '("black" "white" "red" "green" "blue")] ; initial colors, may be changed
             [i (in-naturals 1)])
    (define name (format "color ~a" i)) ; not the label
    (define bt
      (new color-button%
           [keymap-name name]
           [parent bt-panel]
           [color (send the-color-database find-color color)]
           [get-canvas (λ () cv)]
           [callback (λ _ (send cv set-color (get-field color bt)))]))
    bt))

(define bt-erase (new keymapped-button% [parent bt-panel] [label "Clear"]
                      [keymap-name "clear"]
                      [callback (λ (bt ev) (send cv clear-commands))]))

(define bt-undo (new keymapped-button% [parent bt-panel] [label "Undo"]
                     [keymap-name "undo"]
                     [callback (λ (bt ev)
                                 (send cv undo-command))]))

(void (new grow-box-spacer-pane% [parent bt-panel]))

(define bt-open (new keymapped-button% [parent bt-panel] [label "Open"]
                     [keymap-name "open"]
                     [callback
                      (λ (bt ev)
                        (define f
                          (get-file "Open a file" fr #f #f "rktp" '()
                                    '(("Racket Paint" "*.rktp") ("Any" "*.*"))))
                        (when f (send cv open-file f)))]))

(define bt-save (new keymapped-button% [parent bt-panel] [label "Save"]
                     [keymap-name "save"]
                     [callback
                      (λ (bt ev)
                        (define f
                          (put-file "Save file" fr #f #f "rktp" '()
                                    '(("Racket Paint" "*.rktp") ("Any" "*.*"))))
                          (when f (send cv save-file f)))]))

(define bt-shortcuts (new keymapped-button% [parent bt-panel] [label "&Shortcuts"]
                          [keymap-name "shortcuts"]
                          [callback
                           (λ (bt ev)
                             (new search-list-box-frame% [parent fr] [label "Shortcuts"]
                                  [contents
                                   (sort (hash->list (send keymap get-mappings))
                                         string<=? #:key cdr)]
                                  [key (λ (content)
                                         (string-append (~a (simplify-event-dict (car content)))
                                                        "\t"
                                                        (cdr content)))]
                                  [callback (λ (idx lbl content)
                                              (send keymap call-function (cdr content) #f (new key-event%)))]))]))

(define bt-map-function (new keymapped-button% [parent bt-panel] [label "New shortcut"]
                             [keymap-name "new-shortcut"]
                             [callback
                              (λ _
                                (keymap-map-function/frame keymap
                                                           #:parent fr
                                                           #:callback
                                                           (λ (keymap name ev)
                                                             (when ev (save-keymap)))))]))

(define bt-unmap-function (new keymapped-button% [parent bt-panel] [label "Remove shortcut"]
                               [keymap-name "remove-shortcut"]
                               [callback
                                (λ _
                                  (keymap-remove-mapping/frame keymap
                                                               #:parent fr
                                                               #:callback
                                                               (λ (keymap name ev)
                                                                 (when ev (save-keymap)))))]))

(define width-slider (new slider% [parent fr] [label "Line width"]
                          [min-value line-width-min]
                          [max-value line-width-max]
                          [init-value line-width-init]
                          [callback (λ (sl ev)
                                      (send cv set-line-width (send sl get-value)))]))

(define cv (new my-canvas% [parent fr]
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
