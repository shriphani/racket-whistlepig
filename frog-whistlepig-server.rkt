#lang racket

(require net/url
         rackjure
         web-server/http
         web-server/servlet
         web-server/servlet-env)

(require "frog-whistlepig.rkt")

(define site-root "http://blog.shriphani.com")
(define document-path "/Users/shriphani/Documents/blog")

(define (start req)
  (response/xexpr
   `(html (head (title "Shriphani's Blog Search"))
          (body (p ,@(map render-result (process-url (request-uri req))))))))

(define (process-url url)
  (let ((query-keyword
         (filter
          (lambda (q) (equal? (car q) 'q))
          (url-query url))))

    (if (null? query-keyword)
        '()
        (search-index (~> query-keyword
                          car
                          cdr)))))

(define (render-result result)
  `(p (h2 ,(path->site result))))

(define (path->site result)
  (string-replace result document-path site-root))

(serve/servlet start #:port 8090 #:servlet-path "/search")
