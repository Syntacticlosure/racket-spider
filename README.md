# racket-spider
a lightweight http-client library.

# spider/get
(spider/get website #:headers [headers empty] #:proxy [proxy #f])
```racket
(spider/get "http://www.test.com/main") 
(spider/get "https://www.test.com/main") ;; enable ssl
(spider/get "https://www.test.com/main" #:headers '("Host : www.test.com" "Cookie : session=1")) ;;use headers keyword
(display (port->string (spider/get "http://www.test.com/main"))) ;; display the result
```

# spider/post
(spider/post website #:headers [headers post-default-headers] #:data [data #f] #:proxy [proxy #f])

```racket
(spider/post "https://www.test.com/login" #:data (spider/post-form username "name" password "password"))
```

# proxy
Both spider/get and spider/post support http proxy currently,the proxy argument is a list contains your proxy configuration.

proxy = (list address port)

for example:

proxy = (list "127.0.0.1" 1080)

# cookies (deprecated in new versions)
Both spider/get and spider/post will handle cookies automatically:

1) add cookies to headers

2) extract cookies from response headers and save them
