#lang racket/gui

(require "webdav.rkt")

(define files #f)
(define n-files-imported 0)

(define (show-nextcloud-config)
  (define frame (new frame% [label "Nextcloud Configuration"]))
  (define panel-conn (new vertical-panel% (parent frame)))
  (define panel-import (new vertical-panel% (parent frame)
                            (min-height 200)
                            (stretchable-height #f)
                            (style '(deleted))))
  (define form (new group-box-panel%
                             (parent panel-conn)
                             (label "Nextcloud Configuration")))


  (define server (new text-field% (label "Server:")
                    (parent form)
                    (min-width 250)))
  (define user (new text-field% (label "Username:")
                    (parent form)
                    (min-width 250)))
  (define pass (new text-field% (label "Password:")
                    (parent form)
                    (min-width 250)
                    (style '(single password))))
  (define path (new text-field% (label "Photos folder:")
                    (parent form)
                    (min-width 250)))
  

  (define test-conn-button (new button% [parent form]
        [label "Check Connection"]
        [callback (lambda (button event)
                    (let ([server (send server get-value)]
                          [user (send user get-value)]
                          [pass (send pass get-value)]
                          [path (send path get-value)])
                      (set! files (files-to-import server user pass path)))
                    (show-import))]))

  (define import-info (new message%
                           (label "")
                           (min-height 50)
                           (min-width 200)
                           (parent panel-import)))
  
  (define progress-bar (new gauge%
                   (label "Progress")
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

