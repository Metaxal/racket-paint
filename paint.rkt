#lang racket

(require racket/gui/base
         pict)

(provide (all-defined-out))

(define line-width-init 1)

;; TODO:
;; - When adding a command, draw on top of the current bitmap,
;;   On undo, redraw from the start.
;; - Save/load bitmap
;; - draw rectangle, filled-rectangle

(define my-canvas%
  (class canvas%
    (define color "black")
    (define line-width line-width-init)
    (define commands '())
    (define tool 'freehand) ; freehand rectangle filled-rectangle
    
    (define/override (on-event ev)
      (when (send ev get-left-down)
        (when (send ev button-changed? 'left)
          ; start a new line
          (set! commands (cons '() commands)))
        (define pos (cons (send ev get-x) (send ev get-y)))
        (set! commands (cons (cons pos (first commands)) (rest commands)))
        (send this refresh)))
    
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
                [else commands]))))

    (define/public (set-line-width w)
      (set! line-width w)
      (set! commands
        (cons (list 'line-width w) 
              (match commands
                [`((line-width ,w-old) . ,rst) rst] ; replace
                [else commands]))))

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

    (define/public (draw dc
                         #:on-line-width [on-line-width (λ (dc width) (void))]
                         #:on-color [on-color (λ (dc color) (void))])
      (define commands (reverse (send cv get-commands)))
      (send dc set-background "white")
      (send dc clear)
      ; Not efficient to redraw all the lines each time. We should keep the previous
      ; picture and draw on top of it instead.
      (for ([cmd (in-list commands)])
        (match cmd
          [`(line-width ,w)
           (define p (send dc get-pen))
           (send dc set-pen (send p get-color) w 'solid)
           (on-line-width dc w)]
          [`(color ,c)
           (define p (send dc get-pen))
           (send dc set-pen (if (list? c) (apply make-color c) c)
                 (send p get-width) 'solid)
           (on-color dc c)]
          [(? list?)
           (send dc draw-lines cmd)]
          [else (error "Unknown command: " cmd)])))

    (super-new)
    (clear-commands)))

(define fr (new frame% [label "Racket Paint"]
                [width 500] [height 500]))

(define bt-panel (new horizontal-panel% [parent fr] [stretchable-height #f]))

(define bt-erase (new button% [parent bt-panel] [label "Clear"]
                      [callback (λ (bt ev) (send cv clear-commands))]))

(for ([color '("black" "white" "red" "green" "blue")])
  (new button% [parent bt-panel] [label (pict->bitmap (colorize (filled-rectangle 20 20) color))]
       [callback (λ (bt ev) (send cv set-color color))]))
(define bt-color (new button% [parent bt-panel] [label "Color"]
                      [callback (λ (bt ev)
                                  (define c (get-color-from-user))
                                  (when c (send cv set-color c)))]))
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

(define width-slider (new slider% [parent fr] [label "Line width"]
                          [min-value 1] [max-value 100] [init-value line-width-init]
                          [callback (λ (sl ev)
                                      (send cv set-line-width (send sl get-value)))]))

(define cv (new my-canvas% [parent fr]
                [paint-callback
                 (λ (cv dc)
                   (send cv draw dc
                         #:on-line-width
                         (λ (dc w) (send width-slider set-value w))))]))

