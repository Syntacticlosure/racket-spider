#lang racket
(require net/http-client net/url net/uri-codec net/cookies
         (for-syntax syntax/parse))
(provide spider/get spider/post spider/post-form)

(define (attach-cookies original-headers url-form)
  (if (cookie-header url-form)
    (let ()
    (define string-header? (and (not (null? original-headers))
                                (string? (first original-headers))))
    (if string-header?
        (cons (format "Cookie : ~a" (cookie-header url-form))
              original-headers)
        (cons (bytes-append #"Cookie :" (cookie-header url-form))
              original-headers))
      )
    original-headers
      ))
  
(define (helper url-str method headers data proxy)
  (define url-form (string->url url-str))
  (define ssl? (string=? "https" (url-scheme url-form)))
  (define port-parameter (if ssl?
                             443
                             80))
  (define ssl-parameter (if proxy
                            (call-with-values
                             (λ ()
                               (http-conn-CONNECT-tunnel (first proxy)
                                                         (second proxy)
                                                         (url-host url-form)
                                                         port-parameter
                                                         #:ssl? (if ssl? 'auto #f)))
                             list)
                            (if ssl? 'auto #f)))
  (display (attach-cookies headers url-form))
  (define-values (a b result)
    (http-sendrecv (url-host url-form)
                   url-str
                   #:ssl? ssl-parameter
                   #:headers (attach-cookies headers url-form)
                   #:data data
                   #:port port-parameter
                   #:method method))
  (extract-and-save-cookies! b url-form)
  
  (if (bytes=? a #"HTTP/1.1 200 OK")
      result
      (error "Bad Request :" a result)))

(define (spider/get url-str #:headers [headers empty]
                    #:proxy [proxy #f])
  (helper url-str "GET" headers #f  proxy)
  )
(define default-post-headers (list "Content-Type: application/x-www-form-urlencoded"))
(define (spider/post url-str #:headers [headers default-post-headers]
                     #:proxy [proxy #f]
                     #:data [data #f])
  (helper url-str "POST" headers data proxy))

(define-syntax (spider/post-form stx)
  (syntax-parse stx
    [(_ (~seq key:id value) ...) #'(alist->form-urlencoded (list (cons 'key value)
                                                                 ...))]))
  
