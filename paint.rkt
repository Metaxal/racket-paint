#lang racket

(require racket/gui/base
         pict
         search-list-box)

(provide (all-defined-out))

(define line-width-init 1)
(define line-width-min 1)
(define line-width-max 100)

;; TODO:
;; - draw rectangle, filled-rectangle
;; - change background color

(define my-keymap%
  (class keymap%
    
    (define mapped-shortcuts '())
    (define/public (get-shortcuts) mapped-shortcuts)
    
    (define/public (add! name thunk . shortcuts)
      (send keymap add-function name (λ (receiver event) (thunk)))
      (for ([sh (in-list shortcuts)])
        (set! mapped-shortcuts (cons (list sh name) mapped-shortcuts))
        (send this map-function sh name)))

    (define/public (call name)
      (send this call-function name this (new event%)))
    
    (super-new)))

(define keymap (new my-keymap%))

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

    (define/override (on-subwindow-char receiver ev)
      (send keymap handle-key-event receiver ev))
    
    (define/override (on-subwindow-event receiver ev)
      (send keymap handle-mouse-event receiver ev))
    
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
      (send keymap add! "clear" (λ () (clear-commands)) "c:e")
      (send keymap add! "undo" (λ () (undo-command)) "c:z" "u")
      (send keymap add! "increase-brush-size"
            (λ () (set-line-width (min line-width-max (+ line-width 1))))
            "c:+" "x" "wheelup")
      (send keymap add! "decrease-brush-size"
            (λ () (set-line-width (max line-width-min (- line-width 1))))
            "c:-" "y" "wheeldown"))
    
    (super-new)
    (send (send this get-dc) set-smoothing 'aligned)
    (clear-commands)))

(define fr (new frame% [label "Racket Paint"]
                [width 500] [height 500]))

(define bt-panel (new horizontal-panel% [parent fr] [stretchable-height #f]))

(define bt-erase (new button% [parent bt-panel] [label "Clear"]
                      [callback (λ (bt ev) (send cv clear-commands))]))

(define (make-button-color-label color)
  (pict->bitmap (colorize (filled-rectangle 20 20) color)))

(define color-button%
  (class button%
    (init-field get-canvas
                [color (send the-color-database find-color "black")])
    (define/override (on-subwindow-event bt ev)
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
        [else #f]))
    (super-new
     [label (make-button-color-label color)])))

(define init-buttons
  (for/list ([color '("black" "white" "red" "green" "blue")]
             [i (in-naturals 1)])
    (define cbt
      (new color-button%
           [parent bt-panel]
           [color (send the-color-database find-color color)]
           [get-canvas (λ () cv)]))
    (λ ()
      (send keymap add!
            (format "color ~a" i)
            (λ () (send cv set-color (get-field color cbt)))
            (format "~a" i)))))

(define bt-undo (new button% [parent bt-panel] [label "Undo"]
                      [callback (λ (bt ev)
                                  (send cv undo-command))]))

(void (new grow-box-spacer-pane% [parent bt-panel]))

(define bt-open (new button% [parent bt-panel] [label "Open"]
                     [callback
                      (λ (bt ev)
                        (define f
                          (get-file "Open a file" fr #f #f "rktp" '()
                                    '(("Racket Paint" "*.rktp") ("Any" "*.*"))))
                        (when f (send cv open-file f)))]))

(define bt-save (new button% [parent bt-panel] [label "Save"]
                     [callback
                      (λ (bt ev)
                        (define f
                          (put-file "Save file" fr #f #f "rktp" '()
                                    '(("Racket Paint" "*.rktp") ("Any" "*.*"))))
                          (when f (send cv save-file f)))]))

(define bt-shortcuts (new button% [parent bt-panel] [label "&Shortcuts"]
                          [callback
                           (λ (bt ev)
                             (new search-list-box-frame% [parent fr] [label "Shortcuts"]
                                  [contents
                                   (sort (send keymap get-shortcuts)
                                         string<=? #:key second)]
                                  [key (λ (content)
                                         (string-append (~a (first content))
                                                        "\t"
                                                        (second content)))]
                                  [callback (λ (idx lbl content)
                                              (send keymap call (second content)))]))]))

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

(for-each (λ (thk) (thk)) init-buttons) ; must be done once cv has a value
