(use-modules (haunt asset)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt builder rss)
             (haunt builder redirects)
             (haunt builder flat-pages)
             (haunt publisher rsync)
             (haunt publisher sourcehut)
             (haunt reader)
             (haunt reader skribe)
             (haunt reader texinfo)
             (haunt reader commonmark)
             (haunt post)
             (haunt site)
             (markdown)
             (projects)
             (theme)
             (highlight)
             ((redirects) #:prefix xroger88:)
             ((xroger88 reader org-mode) #:prefix xroger88:)
             ((jakob reader org-mode) #:prefix jakob:)
             (utils)
             )

;;(display "hello!")
;;%load-path
;;(add-to-load-path "/gnu/store/49q1k4mi5xg32vayis1iq86vgpwxb43y-haunt-0.3.0/share/guile/site/3.0")
;;(add-to-load-path "/home/admin/Projects/MyDev/haunt-test")

;; run nrepl server in terminal
;; guix shell guile-next guile-ares-rs haunt -- guile -c '((@ (nrepl server) run-nrepl-server))'

;; to connect the nrepl server from emacs
;; in emacs, M-x sesman-start


(define post-prefix "/posts")

(define collections
  `(("Recent Posts" "index.html" ,posts/reverse-chronological)
    ("All Posts" "posts.html" ,identity)))

(site #:title "xroger88"
      #:domain "nationally-stable-sparrow.ngrok-free.app" ;; used in RSS feed .xml
      #:default-metadata
      '((author . "Jongkeun Na")
        (email  . "xroger88@gmail.com"))
      #:readers (list
                 commonmark-reader
                 texinfo-reader
                 skribe-reader
                 sxml-reader
                 html-reader
                 ;;xroger88:org-mode-reader
                 jakob:org-mode-reader
                 )
      #:builders (list (blog #:theme xroger88-theme
                             #:collections collections
                             #:post-prefix post-prefix
                             #:posts-per-page 10)
                       (atom-feed #:blog-prefix post-prefix)
                       (atom-feeds-by-tag #:blog-prefix post-prefix)
                       (rss-feed)
                       ;;(redirects xroger88:redirects)
                       (flat-pages "pages"
                                   #:template (theme-layout xroger88-theme))
                       (static-directory "css")
                       (static-directory "fonts")
                       (static-directory "images"))
       #:publishers (list (rsync-publisher #:name 'rsync
                                           #:destination "/tmp/haunt-example"))
       #:make-slug post-slug-v2)
