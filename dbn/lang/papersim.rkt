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

; I think this is all we need to provide?
(provide dbncolor dbncolor-grey run-paper-sim set-pen-color! clear-paper
         draw-point draw-line get-pixel-color get-pen-color
         get-mouse-x get-mouse-y get-mouse-button get-key get-time dbn-refresh
         dbn-maybe-pause set-antialias dbncolor->greyscale get-bitmap dbn-yield
         dbn-yield/sleep automatically-close)
         

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
    (send paper-frame% on-close)))
    
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

; defines the default bitmap we will modify 
(define current-paper% (gui:make-bitmap (PAPER-WIDTH) (PAPER-HEIGHT)))
(define scratch-paper% (gui:make-bitmap (PAPER-WIDTH) (PAPER-HEIGHT)))
; turns out we actually need two bitmaps
(define (get-bitmap) current-paper%)

; define a new bitmap dc, using the bitmap initialization constructor,
; this allows us to draw onto the canvas (so it sticks) and into the bitmap
(define current-dc% (new gui:bitmap-dc% (bitmap current-paper%)))
(define scratch-dc% (send scratch-paper% make-dc))
                             
; this defines the actual window, note that we create a new event
; space for it because we need to have a thread that's handling
; the retreiving of these events (otherwise the app just sort of
; hangs in loops)
(define newev (make-eventspace))
(current-eventspace newev)
(define paper-frame%
  (new (class gui:frame% (super-new)
         (define/augment (on-close)
           (displayln "")
           (exit)))
       [label "DBN"]
       [width (PAPER-WIDTH)]
       [height (PAPER-HEIGHT)]
       [min-width (PAPER-WIDTH)]
       [min-height (PAPER-HEIGHT)]
       [style '(float hide-menu-bar no-resize-border)]))


; this creates a class that inherits from canvas but passes the handling
; of mouse end keyboard events to a couple of helper functions--we'll create
; the canvas from this instead of from the canvas% object
(define event-handling-canvas%
  (class gui:canvas% (super-new) ; base class
    (define/override (on-event event)
      (handle-mouse-event event))
    (define/override (on-char event)
      (handle-key-event event))))

; create a panel so we can keep the minimum size of the window to the paper size,
; this is where we attach the panel to the frame that was created above
(define main-panel% (new gui:panel% [parent paper-frame%]
     [style '(border)]
     [min-width (+ 2 (PAPER-WIDTH))]
     [min-height (+ 2 (PAPER-HEIGHT))]))

; defines the canvas we draw on--this sets up the callback to
; call draw-bitmap on the dc whenever the canvas needs to be painted,
; and notice that we attach this to the main-panel
(define paper-canvas% (new event-handling-canvas% [parent main-panel%]
     [style '(no-autoclear)]
     [min-width (PAPER-WIDTH)]
     [min-height (PAPER-HEIGHT)]
     [paint-callback
      (λ (canvas dc)
        #;(let-values ([(width height) (send paper-frame% get-client-size)])
          (printf "width: ~a, height: ~a, paper-width ~a, paper-height ~a~n" width
                  height (PAPER-WIDTH) (PAPER-HEIGHT)))
        (send scratch-dc% draw-bitmap current-paper% 0 0)
        (send dc draw-bitmap scratch-paper% 0 0))]))



(define (get-mouse-button)
  mouse-button)

(define (get-mouse-x)
  mouse-x)

(define (get-mouse-y)
  mouse-y)
  
(define mouse-x 0)
(define mouse-y 0)
(define mouse-button 0)

; the beginning of adding mouse recording
(define (handle-mouse-event event)
  (set! mouse-x (send event get-x))
  (set! mouse-y (- (PAPER-HEIGHT) (send event get-y)))
  #;(printf "Mouse (~a, ~a)" mouse-x mouse-y)
  (set! mouse-button (if (eq? (send event button-down?) #t) 100 0)))



(define keys (make-vector 27 0))

(define (set-key! loc val)
  #;(printf "set key ~a to ~a\n" loc val)
  (vector-set! keys loc val))

(define (get-key val)
  (vector-ref keys val))


(define (get-time sym)
  (let* ([the-seconds (current-seconds)]
         [the-date (seconds->date the-seconds)])
    (cond
      [(eq? sym 'hour) (date-hour the-date)]
      [(eq? sym 'minutes) (date-minute the-date)]
      [(eq? sym 'seconds) (date-second the-date)]
      [(eq? sym 'milliseconds) (modulo (current-milliseconds) 1000)]
      [else (raise-argument-error 'getTime "argument must be 'hour, 'minutes, 'seconds, or 'milliseconds" 0 sym)])))
      
    


; the beginning of adding keyboard recording
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
       (set-pen-color! col)
       #;(printf "last-pen: ~a (color: ~a)~n" last-pen col)
       (send current-dc% draw-point xcoord ycoord)
       #;(printf "draw-point at (~a, ~a) with color ~a~n"
               xcoord ycoord col)
       (set-pen-color! last-pen)
       #;(printf "last-pen: ~a (color: ~a)~n" (send current-dc% get-pen) col)
       #;(dbn-refresh))]
    [else (raise-argument-error 'draw-point "dbncolor?" 2 x y col)]))






; force a dbn-refresh on the canvas
(define refresh-timer (current-inexact-milliseconds))
(define (dbn-refresh)
  (let ([cur-time (current-inexact-milliseconds)])
  ; conditionally refresh, only if 1/60th (or 16ms) of a second has passed!
  (when (> (- cur-time refresh-timer) 16)
    #;(printf "refreshing... ~a ~a ~a" cur-time refresh-timer (- cur-time refresh-timer))
    #;(send current-dc% flush)
    #;(send paper-canvas% refresh)
    #;(set! refresh-timer cur-time)
    (send paper-canvas% on-paint)
    #;(send paper-canvas% refresh-now))))

(define (get-dc) (send paper-canvas% get-dc))

; int, int, int, int -> void
; draws a line with the current pen color from x, y to x1, y1.
(define (draw-line x y x1 y1)
  (send current-dc% #;(get-dc) draw-line
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
    (send current-dc% set-smoothing kind)))

; erases the current paper with the given color
(define (clear-paper col)
  (cond
    [(dbncolor? col) (let ([background-color (dbncolor->color% col)]
                           [last-pen (get-pen-color)])
                       ; sets the background color to be the given background color on this
                       ; drawing context
                       ;(send (get-dc) set-background background-color)
                       (send current-dc% set-background background-color)
                       ; clears the drawing region--this dc should be pointing to the
                       ; backing drawing context we are clearing, i.e., the bitmap
                       ;(send (get-dc) clear)
                       (send current-dc% clear)
                       )]
    [else (raise-argument-error 'clear-paper "dbncolor?" 0 col)]))

; sets the current pen color for drawing things
; dbncolor -> void
(define (set-pen-color! col)
  (cond
    [(dbncolor? col) (send current-dc% set-pen (color%->pen% (dbncolor->color% col)))]
    [else (raise-argument-error 'set-pen-color! "dbncolor?" 0 col)]))


; returns the current pen color for drawing things on the canvas
; -> dbncolor
(define (get-pen-color)
  (color%->dbncolor (send (send current-dc% get-pen) get-color)))

; launches a window with the backing bitmap
(define (run-paper-sim)
  (send paper-frame% show #t))

; returns the pixel at a given point
; int, int -> dbncolor
(define (get-pixel-color x y)
  (let ([col% (make-object gui:color%)]
        [xcoord (dbnx->dcx x)]
        [ycoord (dbny->dcy y)])
    (if (send current-dc% get-pixel xcoord ycoord col%)
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
              