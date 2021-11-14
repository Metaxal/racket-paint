#lang racket

(require racket/gui/base
         pict)

(provide (all-defined-out))

(define line-width-init 1)
(define line-width-min 1)
(define line-width-max 100)

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

    (init-field [on-set-line-width (λ (cv width) (void))]
                [background-color "white"])
    
    (define/override (on-event ev)
      (when (send ev get-left-down)
        (when (send ev button-changed? 'left)
          (new-line))
        (add-point (send ev get-x) (send ev get-y))
        (send this refresh)))

    (define/override (on-subwindow-char receiver ev)
      (define code (send ev get-key-code))
      (define ctl (send ev get-control-down))
      (define key (list code))
      (when ctl (set! key (cons 'ctl key)))
      #;(writeln key)
      (define matched?
        (match key
          ['(ctl #\e) (clear-commands)]
          ['(ctl #\z) (undo-command)]
          [(or '(ctl #\+)
               '(#\x)) ; xp-pen "brush-size+"
           (set-line-width (min line-width-max (+ line-width 1)))]
          [(or '(ctl #\-)
               '(#\y)) ; xp-pen "brush-size-"
           (set-line-width (max line-width-min (- line-width 1)))]
          ['(ctl #\1) (set-color "black")]
          ['(ctl #\2) (set-color "white")]
          ['(ctl #\3) (set-color "red")]
          ['(ctl #\4) (set-color "blue")]
          ['(ctl #\5) (set-color "green")]
          [else 'unmatched]))
      (not (eq? matched? 'unmatched))) ; do we not forward the event?
    
    
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

    (define/public (new-line)
      ; start a new line
      (match commands
        [(list-rest '(points) _rst) ; already a new line
         (void)]
        [else (set! commands (cons '(points) commands))]))
    
    (define/public (add-point x y)
      (define pos (cons x y))
      (define-values (prev-points rest-commands)
        (match commands
          [(list-rest (list-rest 'points prev-pts) rst) ;  add to previous list of points
           (values prev-pts rst)]
          [else (values '() commands)]))
      (set! commands
            (cons (list* 'points pos prev-points)
                  rest-commands)))

    (define/public (get-line-width) line-width)

    (define/public (set-line-width w)
      (set! line-width w)
      (set! commands
        (cons (list 'line-width w) 
              (match commands
                [`((line-width ,w-old) . ,rst) rst] ; replace
                [else commands])))
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

    (define/public (draw dc)
      (define commands (reverse (send cv get-commands)))
      (send dc set-background background-color)
      (send dc clear)
      ; Not efficient to redraw all the lines each time. We should keep the previous
      ; picture and draw on top of it instead.
      (for ([cmd (in-list commands)])
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
          [else (error "Unknown command: " cmd)])))

    (super-new)
    (clear-commands)))

(define fr (new frame% [label "Racket Paint"]
                [width 500] [height 500]))

(define bt-panel (new horizontal-panel% [parent fr] [stretchable-height #f]))

(define bt-erase (new button% [parent bt-panel] [label "Clear"]
                      [callback (λ (bt ev) (send cv clear-commands))]))

(define (make-button-color-label color)
  (pict->bitmap (colorize (filled-rectangle 20 20) color)))

(for ([color '("black" "white" "red" "green" "blue")])
  (new button% [parent bt-panel] [label (make-button-color-label color)]
       [callback (λ (bt ev) (send cv set-color color))]))

(define bt-color (new button% [parent bt-panel] [label (make-button-color-label "black")]
                      [callback (λ (bt ev)
                                  (define c (get-color-from-user))
                                  (when c
                                    (send cv set-color c)
                                    (send bt set-label (make-button-color-label c))
                                    (send bt refresh)))]))

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
