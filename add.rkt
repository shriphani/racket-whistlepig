#lang racket

(require racket/file)
(require srfi/13) ; string search methods
(require srfi/14) ; charset library
(require "racket-whistlepig.rkt")

;; Create a new index in /tmp (so this is very much unix only)
;; and add all the racket source files in it
(define (add-source-files)
  (define source-files (filter
                        (lambda (f) (string-contains f ".rkt"))
                        (map path->string (directory-list))))
  (define index (wp-index-create "/tmp/index"))
  (map
   (lambda (file-path)
     (let ((entry (wp-entry-new))
           (f (fopen file-path "r")))
       (begin
         (wp-entry-add-file entry "body" f)
         (wp-index-add-entry index entry)
         (wp-entry-free entry)
         (fclose f)
         (printf
          (format
           "Added ~s in CWD to the index\n"
           file-path)))))
   source-files))

(add-source-files)
