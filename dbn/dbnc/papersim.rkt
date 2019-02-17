#lang racket

(module+ test
  (require rackunit
           rackunit/text-ui)
  )

(require racket/gui)
(require racket/cmdline)
(require racket/gui/dynamic)

; bunch of things to make drawing work
(define gui:make-screen-bitmap (gui-dynamic-require 'make-screen-bitmap))
(define gui:make-bitmap (gui-dynamic-require 'make-bitmap))
(define gui:bitmap-dc% (gui-dynamic-require 'bitmap-dc%))
(define gui:frame% (gui-dynamic-require 'frame%))
(define gui:canvas% (gui-dynamic-require 'canvas%))
(define gui:panel% (gui-dynamic-require 'panel%))
(define gui:color% (gui-dynamic-require 'color%))
(define gui:make-color (gui-dynamic-require 'make-color))
(define gui:make-pen (gui-dynamic-require 'make-pen))

; things the module exports for others to use
(provide dbncolor dbncolor-grey run-paper-sim set-pen-color! clear-paper
         draw-point draw-line get-pixel-color get-pen-color
         get-mouse-x get-mouse-y get-mouse-button get-key get-time dbn-refresh
         dbn-maybe-pause set-antialias dbncolor->greyscale get-bitmap dbn-yield
         dbn-yield/sleep automatically-close PAPER-WIDTH PAPER-HEIGHT
         suspend-drawing resume-drawing)
         

#|
module dbn-papersim: The purpose of this module is to provide the drawing in a fashion that
was expected of design by numbers. We implement this via Racket's drawing toolkit by creating
a frame and a canvas to draw to. 
|#


; handle automatic closing
(define automatically-close (make-parameter #f))
(command-line #:program "dbn"
              #:once-each
              [("-c" "--close") "Automatically close after drawing to the window (useful for testing)"
                                (automatically-close #t)])
              
; by default, the paper-frame will stay open until you close it,
; this method will check command line parameters and call on-close if
; you passed -c to the program--why would you do this? for testing of course!
(define (dbn-maybe-pause)
  (when (automatically-close)
    (send (paper-frame%) on-close)))
    
(define (dbn-yield)
  (yield (current-eventspace)))

(define (dbn-yield/sleep s)
  (sleep/yield s))

; simple structure to represent colors from design by numbers
; The parameter, grey, is rounded to the nearest integer between 0 and 100.
(struct dbncolor (grey) #:transparent
  #:guard (lambda (grey type-name)
            ; I clamp this between 0 and 100
            (if (number? grey) (max 0 (min 100 grey))
                (raise-argument-error 'dbncolor "a number between 0 - 100" 0 grey))))

#;(check-error (dbncolor 1000))


(define PAPER-WIDTH (make-parameter 101))
(define PAPER-HEIGHT (make-parameter 101))

; functions to convert from dbn notation (0 - k) to dc notation (1 - k + 1)
(define (dbnx->dcx x)
   x)

; functions to convert from dbn notation (0 - k) to dc notation (1 - k + 1)
(define (dbny->dcy y)
  (sub1 (- (PAPER-HEIGHT) y)))
                             
; a function that creates the actual window (a frame) based on a width and height
(define (make-paper-frame pwidth pheight)
  (new (class gui:frame% (super-new)
          (define/augment (on-close)
            (displayln "")
            (exit)))
        [label "DBN"]
        [width pwidth]
        [height pheight]
        [min-width pwidth]
        [min-height pheight]
        [style '(float hide-menu-bar no-resize-border)]))

; create a new event space for the window so it has its own thread for handling events
; otherwise the app sort of just hangs in loops
(define newev (make-eventspace))
(current-eventspace newev)
(define paper-frame% (make-parameter #f))



; this creates a class that inherits from canvas but passes the handling
; of mouse end keyboard events to a couple of helper functions--we'll create
; the canvas from this instead of from the canvas% object
(define event-handling-canvas%
  (class gui:canvas%
    ; init the parent class
    (super-new)

    ; init with the width and height of the canvas
    (init width height)

    (define canvas-width width)
    (define canvas-height height)
    ; define a scratch bitmap which we'll draw to
    (define scratch-bitmap (gui:make-bitmap width height))
    ; and get the dc for it
    (define scratch-dc (new gui:bitmap-dc% [bitmap scratch-bitmap]))

    (define scratch-width width)
    (define scratch-height height)
    (define mouse-x 0)
    (define mouse-y 0)
    (define mouse-button 0)

    (define/public (get-paper) scratch-bitmap)
    (define/public (get-paper-dc) scratch-dc)
    (define/public (get-paper-width) scratch-width)
    (define/public (get-paper-height) scratch-height)
    (define/public (get-mouse-x) mouse-x)
    (define/public (get-mouse-y) mouse-y)
    (define/public (get-mouse-button) mouse-button)
    
    ; set up a callback for on-mouse events
    (define/override (on-event event)
      ; store the current mouse coordinates and button state
      (define (handle-mouse-event a-mouse-event)
        ; set the mouse x and y coordinates regardless of the kind of mouse event
        (set! mouse-x (send a-mouse-event get-x))
        ;(if (eq? (paper-canvas%) #f)
        ;    (set! mouse-y (send a-mouse-event get-y))
        (set! mouse-y (- (get-paper-height) (send a-mouse-event get-y)))
        ; if it's a button event, set the mouse button
        (let ([etype (send a-mouse-event get-event-type)])
          (cond
            [(or (eq? etype 'left-down)
                 (eq? etype 'right-down)
                 (eq? etype 'middle-down))
             (set! mouse-button 100)]
            [(or (eq? etype 'left-up)
                 (eq? etype 'right-up)
                 (eq? etype 'middle-up))
             (set! mouse-button 0)])))
      (handle-mouse-event event))

  
      ; setup a callback for on-char events
      (define/override (on-char event)
        (handle-key-event event))

    ; setup to handle on-paint events
    (define/override (on-paint)
      (send (send this get-dc) suspend-flush)
      (send (send this get-dc) draw-bitmap scratch-bitmap 0 0)
      (send (send this get-dc) resume-flush))
    ))

; create a panel so we can keep the minimum size of the window to the paper size,
; this is where we attach the panel to the frame that was created above
(define (make-main-panel panel-parent width height)
  (new gui:panel%
       [parent panel-parent]
       [style '(border)]
       [min-width (+ 2 width)]
       [min-height (+ 2 height)]))

; defines the canvas we draw on--this sets up the callback to
; call draw-bitmap on the dc whenever the canvas needs to be painted,
; and notice that we attach this to the main-panel
(define (make-paper-canvas parent-frame width height)
  ; create a canvas based on the event-handling-canvas% class
  (new event-handling-canvas%
       [width width]
       [height height]
       [parent (make-main-panel parent-frame width height)]
       ;[parent parent-frame]
       [style '(no-autoclear)]
       [min-width width]
       [min-height height]
       ;[paint-callback
        ; the paint callback copies our working bitmap onto the paper.
        ;(λ (canvas dc)
        ;  (send dc draw-bitmap (send get-paper) 0 0))]
       ))

(define paper-canvas% (make-parameter #f))

; get the current drawing context
(define (current-dc%)
  (unless (eq? (paper-canvas%) #f)
    (send (paper-canvas%) get-paper-dc)))

; get the current bitmap we're drawing to
(define (get-bitmap)
  (unless (eq? (paper-canvas%) #f)
    (send (paper-canvas%) get-paper)))

; suspend flushing for drawing
(define (suspend-drawing)
  (unless (eq? (paper-canvas%) #f)
    (send (paper-canvas%) suspend-flush)))

; resume flushing for drawing 
(define (resume-drawing)
  (unless (eq? (paper-canvas%) #f)
    (send (paper-canvas%) resume-flush)))

; returns the state of the left mouse button, 0 for unpressed, 1 for pressed
(define (get-mouse-button)
  (if (eq? (paper-canvas%) #f)
      0
      (send (paper-canvas%) get-mouse-button)))

; returns the x-coordinates of the mouse pointer
(define (get-mouse-x)
  (if (eq? (paper-canvas%) #f)
      0
      (send (paper-canvas%) get-mouse-x)))

; returns the y-coordinates of the mouse pointer
(define (get-mouse-y)
  (if (eq? (paper-canvas%) #f)
      0
      (send (paper-canvas%) get-mouse-y)))
      
; we only the letters, this should be expanded to read more, but it's not clear
; given the language what this would look like
(define keys (make-vector 27 0))

; sets the state of a particular key
(define (set-key! loc val)
  (vector-set! keys loc val))

; gets the state of a particular key
(define (get-key val)
  (vector-ref keys val))

; return the current time in form that DBN expects
(define (get-time sym)
  (let* ([the-seconds (current-seconds)]
         [the-date (seconds->date the-seconds)])
    (cond
      [(eq? sym 'hour) (date-hour the-date)]
      [(eq? sym 'minutes) (date-minute the-date)]
      [(eq? sym 'seconds) (date-second the-date)]
      [(eq? sym 'milliseconds) (modulo (current-milliseconds) 1000)]
      [else (raise-argument-error 'getTime "argument must be 'hour, 'minutes, 'seconds, or 'milliseconds" 0 sym)])))
        

; function used whenever there's a keyboard event detected by the window
; here we store the state whenever it occurs
(define (handle-key-event event)
  (let ([press (send event get-key-code)]
        [release (send event get-key-release-code)])
    (cond
      [(char? press) (let ([val (- (char->integer (char-upcase press)) 64)])
                         (cond
                           [(and (>= val 0) (<= val 26)) (set-key! val 100)]))]
      [(char? release) (let ([val (- (char->integer (char-upcase release)) 64)])
                           (cond
                             [(and (>= val 0) (<= val 26)) (set-key! val 0)]))])))
 

; int int dbncolor -> void
; draws a point in a specific dbncolor on the given coordinate
(define (draw-point x y col)
  (cond
    [(dbncolor? col)
     (let ([last-pen (get-pen-color)]
           [xcoord (dbnx->dcx x)]
           [ycoord (dbny->dcy y)])
       ; set the pen color if the last pen color isn't the same
       (unless (eq? last-pen col)
         (set-pen-color! col))
       ; using the (current-dc%), draw the point at the coordinate
       (send (current-dc%) draw-point xcoord ycoord)
       #;(printf "draw-point at (~a, ~a) with color ~a~n"
               xcoord ycoord col)
       (set-pen-color! last-pen)
       #;(printf "last-pen: ~a (color: ~a)~n" (send (current-dc%) get-pen) col)
       #;(dbn-refresh))]
    [else (raise-argument-error 'draw-point "dbncolor?" 2 x y col)]))






; force a dbn-refresh on the canvas
(define refresh-timer (current-inexact-milliseconds))
(define (dbn-refresh)
  (let ([cur-time (current-inexact-milliseconds)])
  ; conditionally refresh, only if 1/60th (or 16ms) of a second has passed!
  (when (> (- cur-time refresh-timer) 16)
    #;(printf "refreshing... ~a ~a ~a" cur-time refresh-timer (- cur-time refresh-timer))
    #;'()
    #;(send (current-dc%) flush)
    ;(send (paper-canvas%) refresh-now)
    #;(set! refresh-timer cur-time)
    ; this was to force drawing...
    (send (paper-canvas%) on-paint)
    ;(send (paper-canvas%) refresh)
    #;(send (paper-canvas%) refresh-now))))

;(define (get-dc) (send (paper-canvas%) get-dc))

; int, int, int, int -> void
; draws a line with the current pen color from x, y to x1, y1.
(define (draw-line x y x1 y1)
  (send (current-dc%) draw-line
        (dbnx->dcx x)
        (dbny->dcy y)
        (dbnx->dcx x1)
        (dbny->dcy y1)))


; turns on or off antialiasing
(define (set-antialias val)
  (let ([kind (cond
                [(= val 0) 'unsmoothed]
                [(= val 1) 'smoothed]
                [(= val 2) 'aligned])])
    (send (current-dc%) set-smoothing kind)))

; erases the current paper with the given color
(define (clear-paper col)
  (cond
    [(dbncolor? col) (let ([background-color (dbncolor->color% col)]
                           [last-pen (get-pen-color)])
                       ; sets the background color to be the given background color on this
                       ; drawing context
                       ;(send (get-dc) set-background background-color)
                       (send (current-dc%) set-background background-color)
                       ; clears the drawing region--this dc should be pointing to the
                       ; backing drawing context we are clearing, i.e., the bitmap
                       ;(send (get-dc) clear)
                       (send (current-dc%) clear)
                       )]
    [else (raise-argument-error 'clear-paper "dbncolor?" 0 col)]))

; sets the current pen color for drawing things
; dbncolor -> void
(define (set-pen-color! col)
  (cond
    [(dbncolor? col) (send (current-dc%) set-pen (color%->pen% (dbncolor->color% col)))]
    [else (raise-argument-error 'set-pen-color! "dbncolor?" 0 col)]))


; returns the current pen color for drawing things on the canvas
; -> dbncolor
(define (get-pen-color)
  (color%->dbncolor (send (send (current-dc%) get-pen) get-color)))

; sets things up and launches a window with the backing bitmap
(define (run-paper-sim width height)
  ; sanity checking
  (when (or (< width 0) (< height 0))
    (raise-arguments-error 'run-paper-sim "Width and height should be greater than 0"
                           "width" width "height" height))
  ; helper to set up the paper frame
  (define (setup)
    ; now set up the paper frame first
    (paper-frame% (make-paper-frame width height))
    (paper-canvas% (make-paper-canvas (paper-frame%) width height))
    (send (paper-frame%) show #t))
  ; configure the width and height if needed, which means recreating the window and canvas
  (cond
    ; if the paper size has changed, we have to set it here and run setup again
    [(not (and (= (PAPER-WIDTH) width)
               (= (PAPER-HEIGHT) height)))
     (PAPER-WIDTH width)
     (PAPER-HEIGHT height)
     (unless (eq? (paper-frame%) #f)
       (send (paper-frame%) show #f))
     (setup)]
    ; or if the paper frame is #f, it hasn't been setup yet, so let's run it
    [(eq? (paper-frame%) #f) (setup)]
    [else
     ; otherwise, just be sure it's showing
     (send (paper-frame%) show #t)]))
     
    
; returns the pixel at a given point
; int, int -> dbncolor
(define (get-pixel-color x y)
  (let ([col% (make-object gui:color%)]
        [xcoord (dbnx->dcx x)]
        [ycoord (dbny->dcy y)])
    (when (or (< xcoord 0) (> xcoord (PAPER-WIDTH)))
      (raise-argument-error 'get-pixel-color "x must be between 0 and PAPER-WIDTH" x))
    (when (or (< ycoord 0) (> ycoord (PAPER-HEIGHT)))
      (raise-argument-error 'get-pixel-color "y must be between 0 and PAPER-HEIGHT" y))
    (if (send (current-dc%) get-pixel xcoord ycoord col%)
        (let ([red (send col% red)]
              [green (send col% green)]
              [blue (send col% blue)]
              [alpha (send col% alpha)])
          #;(printf "rbg: (~a, ~a, ~a, ~a) at (~a, ~a) (was ~a ~a)~n"
                  red green blue alpha
                  xcoord ycoord x y)
          (dbncolor-grey (color%->dbncolor col%)))
        (raise-arguments-error 'get-pixel-color "x and y do not refer to a valid pixel"
                               "x" xcoord "y" ycoord))))
    

; dbncolor -> color%
; creates a dbncolor from the range 0-100 to be a greyscale
; color% used by racket gui
(define (dbncolor->color% col)
  (let ([x (- 255 (min 255 (max 0 (exact-round (* (dbncolor-grey col) 2.55)))))])
    (gui:make-color x x x 1)))

; color -> dbncolor
; convert an racket gui color% into a dbncolor
(define (color%->dbncolor col)
  (let ([red (send col red)]
        [green (send col green)]
        [blue (send col blue)])
    ; luminosity conversion for greyscale: 0.21 R + 0.72 G + 0.07 B.
    (dbncolor (- 100 (min 100 (max 0 (exact-round
              (* (+ (* .21 (/ red 255)) (* .72 (/ green 255)) (* .07 (/ blue 255))) 100))))))))

; converts a color% to a pen%, which is needed for drawing in the gui
(define (color%->pen% col)
  (gui:make-pen #:color col #:width 1))

; convert a dbncolor to the grey scale value in the interval [0, 100]
(define (dbncolor->greyscale col)
  (dbncolor-grey col))
              