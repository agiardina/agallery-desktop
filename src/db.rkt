#lang racket

(require sql)
(require db)
(require "config.rkt")

(define sql-create-photos "CREATE table photos (
id INTEGER PRIMARY KEY AUTOINCREMENT,
path TEXT,
origin TEXT,
width INTEGER,
height INTEGER,
size INTEGER,
date_time_original TEXT,
exif TEXT)")

(make-directory* base-path)
(define sql-photo-table-exists "SELECT name FROM sqlite_master WHERE type='table' AND name='photos'")
(define db-conn (sqlite3-connect #:mode 'create #:database (string-append base-path "/db")))

(when (not (query-maybe-value db-conn sql-photo-table-exists))
  (query-exec db-conn sql-create-photos))

(provide db-conn)
