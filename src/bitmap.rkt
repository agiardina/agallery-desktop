#lang racket

(require mediafile)
(require json)
(require racket/serialize)
(require racket/draw)

(define bmp->info
  (case-lambda
    [(path) (bmp->info path (read-bitmap path))]
    [(path bmp)
     (define props (mediafile-props (path->mediafile path)))
     (define hash-props
       (make-hash(for/fold ([acc '()])
                           ([p props])
                   (append acc (cdr p)))))
     (hash 'width (send bmp get-width)
           'height (send bmp get-height)
           'size (file-size path)
           'date-time-original (hash-ref hash-props 'exif:date-time-original "")
           'exif hash-props)
     ]))

(provide bmp->info)

