#lang typed/racket
(require typed/net/url typed/net/uri-codec typed/openssl
         (for-syntax syntax/parse))

(define-type Base-SSL (U False Symbol SSL-Client-Context))
(define-type Base-SSL-Tnc (U Base-SSL
                             (List Base-SSL Input-Port Output-Port (-> Port Void))))
(define-type BString (U Bytes String))

(require/typed net/http-client
               [http-conn-CONNECT-tunnel
                (->* ((U Bytes String)
                      Integer
                      (U Bytes String)
                      Integer)
                     (#:ssl? Base-SSL)
                     (Values Base-SSL
                             Input-Port
                             Output-Port
                             (-> Port Void)))]
               [http-sendrecv
                (->* [BString BString]
                     [#:ssl? Base-SSL-Tnc #:port Positive-Integer #:version BString
                      #:method (U BString Symbol) #:headers (Listof BString) #:data (Option BString) 
                      #:content-decode (Listof Symbol)]
                     (values Bytes (Listof Bytes) Input-Port))])
                                              
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
  (define host (let ([t (url-host url-form)])
                 (if t t
                     (error 'spider
                            "spider.rkt : couldn't get host"))))
  (define ssl? (string=? "https" (let ([scheme (url-scheme url-form)])
                                   (if scheme scheme
                                       (error 'spider
                                              "spider.rkt : couldn't get protocol of request"
                                              url-str)))))
  (define port-parameter (if ssl?
                             443
                             80))
  (define ssl-parameter (if proxy
                            (let-values ([(v1 v2 v3 v4)
                                          (http-conn-CONNECT-tunnel (first proxy)
                                                                    (second proxy)
                                                                    (url-host url-form)
                                                                    port-parameter
                                                                    #:ssl? (if ssl? 'auto #f))])
                              (list v1 v2 v3 v4))
                            (if ssl? 'auto #f)))
  (define-values (a b result)
    (http-sendrecv host
                   url-str
                   #:ssl? ssl-parameter
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
