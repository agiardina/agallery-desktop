#lang racket/gui

(require "webdav.rkt")
(require framework/gui-utils)

(define files #f)
(define n-files-imported 0)

(define (show-nextcloud-config)
  (define frame (new frame%
                     [label "AGallery"]
                     (style (list 'no-resize-border))
                     (min-width 390)))
  (define panel-conn (new vertical-panel% (parent frame)))
  (define panel-import (new vertical-panel% (parent frame)
                            (min-height 200)
                            (stretchable-height #f)
                            (style '(deleted))))
  (define form (new vertical-panel%
                    (parent panel-conn)
                    (stretchable-width #f)
                    ))

  (define vertical-space (new panel% (parent form) (min-height 20)))
  (define row-title (new horizontal-panel% (parent form) (min-height 30) (alignment '(left center))))
  (define icon (new message% (parent row-title) (label (read-bitmap "img/nextcloud-icon.png" #:try-@2x? #t))))
  (define title (new message% (parent row-title)
                     (label "Add a Nextcloud configuration")
                     (font (make-font #:weight 'bold))
                     ))
  
  (define row0 (new horizontal-panel% (parent form) (min-height 40) (alignment '(left center))))
  (define intro (new message% (parent row0)
                     (label "To get started, fill out the following information:")
                     (font small-control-font)))
  
  (define row1 (new horizontal-panel% (parent form) (alignment '(right center))))
  (define server-label (new message% (parent row1) (label "Server:")))
  (define server (new text-field% (label #f)
                      (parent row1)
                      (min-width 250)
                      (stretchable-width #f)))

  (define row2 (new horizontal-panel% (parent form) (alignment '(right center))))  
  (define user-label (new message% (parent row2) (label "Username:")))
  (define user (new text-field% (label #f)
                    (parent row2)
                    (min-width 250)
                    (stretchable-width #f)))
  (define row3 (new horizontal-panel% (parent form) (alignment '(right center))))
  (define pass-label (new message% (parent row3) (label "Password:")))
  (define pass (new text-field% (label #f)
                    (parent row3)
                    (min-width 250)
                    (stretchable-width #f)
                    (style '(single password))))
  (define row4 (new horizontal-panel% (parent form) (alignment '(right center))))
  (define path-label (new message% (parent row4) (label "Photos folder:")))
  (define path (new text-field% (label #f)
                    (parent row4)
                    (min-width 250)
                    (stretchable-width #f)
                    ))
  
  (define row-btn (new horizontal-panel% (parent form) (alignment '(right center)) (min-height 50)))
  (define test-conn-button (new button% [parent row-btn]
        [label "Add server"]
        [callback (lambda (button event)
                    (let ([server (send server get-value)]
                          [user (send user get-value)]
                          [pass (send pass get-value)]
                          [path (send path get-value)])
                      (set! files
                            (gui-utils:show-busy-cursor
                             (lambda ()
                               (take (files-to-import server user pass path) 10)))))
                    (show-import))]))

  (define import-info (new message%
                           (label "")
                           (min-height 50)
                           (min-width 200)
                           (parent panel-import)))
  
  (define progress-bar (new gauge%
                            (label "Progress")
                            (min-height 50)
                   (parent panel-import)
                   (range 100)))

  (define (increase-progress-bar)
    (set! n-files-imported (+ n-files-imported 1))
    ;(displayln (string-append  "Imported " (number->string n-files-imported) "/" (number->string (length files))))
    (send progress-bar set-value n-files-imported))

  (define import-button (new button% [parent panel-import]
        [label "Import Photos"]
        [callback (lambda (button event)
                    (let ([server (send server get-value)]
                          [user (send user get-value)]
                          [pass (send pass get-value)])
                      (import-files server user pass files increase-progress-bar))
                    )]))

  (define (show-import)
    (send import-info set-label
          (string-append "There are " (number->string (length files)) " files to import. Do you want to import them?"))
    (send progress-bar set-range (length files))
    (send frame delete-child panel-conn)
    (send frame add-child panel-import))
  
  
  (send frame show #t))

(show-nextcloud-config)

