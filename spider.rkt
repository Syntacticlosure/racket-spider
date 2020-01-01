#lang typed/racket
(require typed/net/http-client typed/net/url typed/net/uri-codec
         typed/openssl typed/racket/unsafe
         (for-syntax syntax/parse))
(unsafe-require/typed racket/base [(values unsafe-cast) (All (a b) (-> a b))])
(define-type BaseSSL (U False Symbol SSL-Client-Context))
(require/typed net/http-client [http-conn-CONNECT-tunnel
                                (->* ((U Bytes String)
                                      Integer
                                      (U Bytes String)
                                      Integer)
                                     (#:ssl? BaseSSL)
                                     (Values BaseSSL
                                             Input-Port
                                             Output-Port
                                             (-> Port Void)))])
                                              
(provide spider/get spider/post spider/post-form)
(define-type ProxyType (U (List String Integer) False))
(define-type HeadersType (U (Listof String) (Listof Bytes)))
(define-type DataType (U String Bytes False))

(define (helper [url-str : String]
                [method : String]
                [headers : HeadersType]
                [data : DataType]
                [proxy : ProxyType])
  (define url-form (string->url url-str))
  (define ssl? (string=? "https" (let ([scheme (url-scheme url-form)])
                                   (if scheme scheme
                                       (error 'spider
                                              "spider.rkt : couldn't get protocol of request"
                                              url-str)))))
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
  (define-values (a b result)
    (http-sendrecv (url-host url-form)
                   url-str
                   #:ssl? (unsafe-cast ssl-parameter) ; hacks
                   #:headers headers
                   #:data data
                   #:port port-parameter
                   #:method method))
  
  (if (bytes=? a #"HTTP/1.1 200 OK")
      result
      (raise-user-error "Bad Request :" url-str a (port->string result))))

(define default-post-headers : HeadersType
  (list "Content-Type: application/x-www-form-urlencoded"))

(define (spider/get [url-str : String]
                    #:headers [headers : HeadersType empty]
                    #:proxy [proxy : ProxyType #f]
                    )
  (helper url-str "GET" headers #f  proxy))

(define (spider/post [url-str : String]
                     #:headers [headers : HeadersType default-post-headers]
                     #:proxy [proxy : ProxyType #f]
                     #:data [data : DataType #f])
  (helper url-str "POST" headers data proxy))

(define-syntax (spider/post-form stx)
  (syntax-parse stx
    [(_ (~seq key:id value) ...) #'(alist->form-urlencoded (list (cons 'key value)
                                                                 ...))]))