#lang racket

(require racket/cmdline)
(require "racket-whistlepig.rkt")

(define (test-index-load)
  (wp-index-load "/tmp/index"))

(define (query index q num-res)
  (define query (wp-query-parse q "body"))
  (define query2 (wp-index-setup-query index query))
  (wp-index-run-query index query2 num-res))

(define (main)
  (define (inner index)
    (printf "query: ")
    (let ((q (read-line (current-input-port))))
      (begin
        (map
         (lambda (x) (printf "found doc ~a\n" x))
         (second (query index q 100)))
        (inner index))))
  (let* ((index-prefix (command-line
                        #:program "query"
                        #:args (index-prefix)
                        index-prefix))
         
         (index        (wp-index-load index-prefix)))
    
    (inner index)))

(main)
