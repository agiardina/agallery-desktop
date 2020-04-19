 #lang racket/gui

(require racket/date)
(require racket/future)
(require "photos.rkt")
(require "lib.rkt")
(require "widgets.rkt")


(define n-thumbs (vector-length photos-ids))

(define frame (new frame% [label "Photo Gallery"]
                   [style (list 'no-resize-border 'fullscreen-button)]))

(define main-panel (new vertical-pane% [parent frame]))

(define toolbar-pane (new pane% [parent main-panel]
                          [min-height 38]
                          [stretchable-height #f]))

(define toolbar-back (new canvas% [parent toolbar-pane] 
                          [style (list 'transparent)]
                          [paint-callback (lambda (canvas dc) 
                                            (send dc set-brush (make-color 42 47 59) 'solid)
                                            (send dc set-brush gradient-brush)
                                            (send dc set-pen "black" 1 'transparent)
                                            (send dc draw-rectangle 0 0 (send canvas get-width) (send canvas get-height))
                                            (send dc set-pen (make-color 178 178 178) 1 'solid)
                                            (send dc draw-line 0 (- (send canvas get-height) 1) 
                                                  (send canvas get-width) (- (send canvas get-height) 1))
                                            )]))

(define toolbar (new horizontal-panel% [parent toolbar-pane] 
        [border 2]
        [stretchable-height #f]
        [alignment (list 'left 'center)]))

(make-full-spacer toolbar)
(make-toolbar-button toolbar "favorite" displayln)
(make-toolbar-button toolbar "add" displayln)

(define body (new horizontal-panel% [parent main-panel]))
(define sidebar-pane (new pane% [parent body] 
							[min-width 200]))
(define sidebar-back (new canvas% [parent sidebar-pane] 
                          [min-width 200]
                          [stretchable-width #f]
                          [style (list 'transparent)]))

(define sidebar (new vertical-panel% [parent sidebar-pane]))
(define gallery-label (new sidebar-title% [parent sidebar] [label "Gallery"]))
(define photos-label (new sidebar-item% [parent sidebar] [label "Photos"]))
(define albums-label (new sidebar-title% [parent sidebar] [label "Albums"]))
(define thumbs-vector (make-parameter '()))
(define current-photo (make-parameter #f))
(define repaint #t)

(define thumb-width 160)
(define thumb-height 160)
(define row-height 170)
(define col-width 170)
(define n-rows 5)
(define n-cols 7)
(define state-machine "thumbs")
(define last-y -1)
(define scroll-y 0)
(define speed 0)

(define (paint-thumbs dc canvas)
  (send dc clear)
  (for ([t (thumbs-vector)])
    (when (and t (not (void? t)))
      (let ([id (hash-ref t "id")])
        (send dc draw-bitmap (hash-ref t "bitmap") (hash-ref t "x") (hash-ref t "y"))
        (when (eq? id (current-photo))
          (send dc set-brush "black" 'transparent)
          (send dc set-pen (make-color 37 101 217) 3 'solid)
          (send dc set-smoothing 'smoothed)
          (send dc draw-rounded-rectangle (- (hash-ref t "x") 3)
                (- (hash-ref t "y") 3) 
                (+ (hash-ref t "width") 6) 
                (+ (hash-ref t "height") 6)
                -0.025
                ))))))

(define (paint-photo dc canvas)
  (let* ([photo (get-photo (current-photo) 960)]
         [canvas-w (send canvas get-width)]
         [canvas-h (send canvas get-height)]
         [photo-w (send photo get-width)]
         [photo-h (send photo get-height)]
         [ratio-w (/ canvas-w photo-w)]
         [ratio-h (/ canvas-h photo-h)]) 

    (send dc clear)

    (if (> ratio-w ratio-h)
        (let* ([width (floor (* photo-w ratio-h))]
               [height canvas-h]
               [left (floor (/ (- canvas-w width) 2))]) 										
          (send dc draw-bitmap (scale-bitmap photo width height) left 0))
        (let* ([width canvas-w]
               [height (floor (* photo-h ratio-w))]
               [top (floor (/ (- canvas-h height) 2))])
          (send dc draw-bitmap (scale-bitmap photo width height) 0 top)))))

(define (paint-callback canvas dc)
	(case state-machine
		[("thumbs") (paint-thumbs dc canvas)]
		[("photo") (paint-photo dc canvas)]))

(define (make-photo-thread) 
  (thread
   (lambda ()
     (let loop ()
       (let ([ids (thread-receive)])
         (when (not (empty? ids))
           (for/async ([id ids])
             (get-photo id 320))
           (set! repaint #t)))
       (loop)))))

(define photo-thread (make-photo-thread))

(define (paint canvas scroll-y)

  (let* ([min-width (* n-cols col-width)]
         [min-height (* (- n-rows 1) row-height)]
         [above-rows (quotient scroll-y row-height)]
         [above-thumbs (* above-rows n-cols)])

    (kill-thread photo-thread) 
    (set! photo-thread (make-photo-thread))
    (thread-suspend photo-thread)
    (thumbs-vector 
     (for/fold 
         ([acc '()]) 
         ([row (in-range 0 n-rows)])
       (append acc (for/list ([col (in-range 0 n-cols)])
                     (let ([i (+ above-thumbs col (* row n-cols))]) 
                       (when (< i n-thumbs) 
                         (let-values ([(bitmap resolution) (get-best-thumb-by-pos i)])
                           (let* (
                                  [id (pos->id i)]
                                  [x (* col col-width)]
                                  [y (- (* row row-height) (remainder scroll-y row-height))]
                                  [scaled-bitmap (scale-bitmap bitmap thumb-width)]
                                  [offset-x (/ (- thumb-width (send scaled-bitmap get-width)) 2)]
                                  [offset-y (/ (- thumb-width (send scaled-bitmap get-height)) 2)]
                                  [thumb (hash  "id" id
                                                "resolution" resolution
                                                "bitmap" scaled-bitmap 
                                                "x" (+ offset-x x) 
                                                "y" (+ offset-y y)
                                                "width" (send scaled-bitmap get-width)
                                                "height" (send scaled-bitmap get-height))])
                             thumb
                             ))))))))

    (send canvas refresh-now)

    (thread-resume photo-thread)
    (thread-send photo-thread 		
                 (map 
                  (lambda (t) (hash-ref t "id"))
                  (filter (lambda (t)
                            (and (hash? t) (eq? 60 (hash-ref t "resolution")))                     ) 
                          (thumbs-vector))))))

(define (get-clicked-photo canvas x y)
  (for ([t (thumbs-vector)])
    (when (and t (hash? t)) 
      (let* ([id (hash-ref t "id")]
             [x-start (hash-ref t "x")]
             [y-start (hash-ref t "y")]
             [x-end (+ x-start (hash-ref t "width"))]
             [y-end (+ y-start (hash-ref t "height"))])
        (when (and (>= x x-start )
                   (>= y y-start )
                   (<= x x-end )
                   (<= y y-end ))
          (current-photo id)
          (send canvas refresh-now))))))

(define (next-photo)
  (when (current-photo)
    (let* ([pos (id->pos (current-photo))]
           [next-pos (+ 1 pos)]
           [next-id (pos->id next-pos)])
      (when next-id 
        (current-photo next-id)
        (send canvas refresh-now)))))

(define gallery%
  (class canvas%

    (define scroll 0)

    (define/override (on-scroll event)
      (set! scroll-y (send event get-position))
      (super on-scroll event))

    (define/override (on-event event)
      (when (eq? 'left-down (send event get-event-type))
        (let ([x (send event get-x)]
              [y (send event get-y)])
          (get-clicked-photo this x y))))

    (define/override (on-char event)
      (let ([key (send event get-key-code)])
        (case key
          [(eq? key #\space) (if (eq? state-machine "photo")
                                 (set! state-machine "thumbs")
                                 (set! state-machine "photo"))
                             (send this refresh-now)]
          ['escape (set! state-machine "thumbs") 
                   (send this refresh-now)]
          ['wheel-down (set! speed (+ speed 1))]
          ['wheel-up (set! speed (- speed 1))]
          ['right (next-photo)])))
    
    (super-new)))

(define canvas (new gallery% [parent body]
                    [min-width (* n-cols col-width)]
                    [min-height (* (- n-rows 1) row-height)]
                    [style (list 'vscroll 'no-autoclear)]
                    [paint-callback paint-callback]))

(define timer
  (new timer%
       (interval 10)
       (notify-callback
        (lambda ()
          (cond 
            [(not (= last-y scroll-y)) 
             (paint canvas scroll-y)]
            [(not (= speed 0)) 
             (set! scroll-y (max 0 (+ last-y (* speed 8))))
             (set! speed 0)
             (paint canvas scroll-y)]
            [repaint 
             (set! repaint #f)
             (paint canvas last-y)])
          (set! last-y scroll-y)))))

(send canvas init-manual-scrollbars 1050 (* row-height (+ 1 (quotient n-thumbs n-cols))) 100 100 0 0)

(send frame show #t)
