;;; Copyright © 2018-2021 David Thompson <davet@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (theme)
  #:use-module (haunt artifact)
  #:use-module (haunt builder blog)
  #:use-module (haunt html)
  #:use-module (haunt post)
  #:use-module (haunt site)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (utils)
  #:export (xroger88-theme
            flat-page-template
            static-page
            project-page))

(define %cc-by-sa-link
  '(a (@ (href "https://creativecommons.org/licenses/by-sa/4.0/"))
      "Creative Commons Attribution Share-Alike 4.0 International"))

(define %cc-by-sa-button
  '(a (@ (class "cc-button")
         (href "https://creativecommons.org/licenses/by-sa/4.0/"))
      (img (@ (src "https://licensebuttons.net/l/by-sa/4.0/80x15.png")))))

(define (first-paragraph post)
  (let loop ((sxml (post-sxml post)))
    (match sxml
      (() '())
      (((and paragraph ('p . _)) . _)
       (list paragraph))
      ((head . tail)
       (cons head (loop tail))))))

(define xroger88-theme
  (theme #:name "xroger88"
         #:layout
         (lambda (site title body)
           `((doctype "html")
             (head
              (meta (@ (charset "utf-8")))
              (meta (@ (name "viewport")
                       (content "width=device-width, initial-scale=1")))
              (title ,(string-append title " — " (site-title site)))
              (link (@ (rel "alternate")
                       (type "application/atom+xml")
                       (title "Atom feed")
                       (href "/feed.xml")))
              ,(stylesheet "reset")
              ,(stylesheet "fonts")
              ,(stylesheet "xroger88"))
             (body
              (div (@ (class "container"))
                   (nav
                    (ul (li ,(link "xroger88" "/")))
                    (ul (li ,(link "About" "/about.html"))
                        (li ,(link "Blog" "/index.html"))
                        (li ,(link "Projects" "/projects.html"))))
                   ,body
                   (footer (@ (class "text-center"))
                           (p (@ (class "copyright"))
                              "© 2024 Jongkeun Na"
                              ,%cc-by-sa-button)
                           (p "The text and images on this site are
free culture works available under the " ,%cc-by-sa-link " license.")
                           (p "This website is built with "
                              (a (@ (href "https://dthompson.us/projects/haunt.html"))
                                 "Haunt")
                              ", a static site generator written in "
                              (a (@ (href "https://gnu.org/software/guile"))
                                 "Guile Scheme")
                              ".")
                           (p
                            "Follow me on "
                            (a (@ (rel "me")
                                  (href "https://mas.to/@polycrafer"))
                               "Mastodon")
                            "."))))))
         #:post-template
         (lambda (post)
           `((article
              (h1 (@ (class "title")),(post-ref post 'title))
              (div (@ (class "date"))
                   ,(date->string (post-date post)
                                  "~B ~d, ~Y"))
              (div (@ (class "tags"))
                   "Tags:"
                   (ul ,@(map (lambda (tag)
                                `(li (a (@ (href ,(string-append "/feeds/tags/"
                                                                 tag ".xml")))
                                        ,tag)))
                              (assq-ref (post-metadata post) 'tags))))
              (div (@ (class "post"))
                   ,(post-sxml post)))))
         #:collection-template
         (lambda (site title posts prefix)
           (define (post-uri post)
             (string-append prefix "/" (site-post-slug site post) ".html"))

           `((h1 ,title
                 (a (@ (href "/feed.xml"))
                    (img (@ (class "feed-icon") (src "images/feed.png")))))
             ,(map (lambda (post)
                     (let ((uri (post-uri post)))
                       `(article (@ (class "summary"))
                                 (h2 (a (@ (href ,uri))
                                        ,(post-ref post 'title)))
                                 (div (@ (class "date"))
                                      ,(date->string (post-date post)
                                                     "~B ~d, ~Y"))
                                 (div (@ (class "post"))
                                      ,(first-paragraph post))
                                 (a (@ (href ,uri)) "read more →"))))
                   posts)))
         #:pagination-template
         (lambda (site body previous-page next-page)
           `(,@body
             (div (@ (class "paginator"))
                  ,(if previous-page
                       `(a (@ (class "paginator-prev") (href ,previous-page))
                           "← Newer")
                       '())
                  ,(if next-page
                       `(a (@ (class "paginator-next") (href ,next-page))
                           "Older →")
                       '()))))))

(define (flat-page-template site metadata body)
  ((theme-layout dthompson-theme) site (assq-ref metadata 'title) body))

(define (static-page title file-name body)
  (lambda (site posts)
    (serialized-artifact file-name
                         (with-layout dthompson-theme site title body)
                         sxml->html)))

(define* (project-page #:key name file-name description usage requirements
                       installation manual? license repo releases guix-package
                       (irc-channel "#guile"))
  (define (tarball-url version)
    (string-append "https://files.dthompson.us/releases/"
                   repo "/" repo "-" version
                   ".tar.gz"))
  (define body
    `((h1 ,name)
      ,description
      ,@(if usage
            `((h2 "Usage") ,usage)
            '())
      ,@(if manual?
         `((h2 "Documentation")
           (p ,(anchor "View the reference manual"
                       (string-append "https://files.dthompson.us/docs/"
                                      repo "/latest"))))
         '())
      (h2 "Releases")
      (ul ,(map (match-lambda
                  ((version date)
                   (let ((url (tarball-url version)))
                     `(li ,(date->string date "~Y-~m-~d")
                          " — " ,version " — "
                          ,(anchor (string-append repo "-" version ".tar.gz")
                                   url)
                          " — "
                          ,(anchor "GPG signature"
                                   (string-append url ".asc"))))))
                releases))
      (h2 "Requirements")
      (ul ,(map (lambda (requirement)
                  `(li ,requirement))
                requirements))
      (h2 "Installation")
      ,@(if installation
            (list installation)
            (match (car releases)
              ((version _)
               `(,@(if guix-package
                       `((p "To install " ,name " with the GNU Guix package manager, run:")
                         (pre "guix install " ,guix-package))
                       '())
                 (p "To build and install " ,name " from source, run:")
                 (pre "wget "
                      ,(tarball-url version)
                      "
tar xf "
                      ,repo "-" ,version ".tar.gz"
                      "
cd "
                      ,repo "-" ,version
                      "
./configure
make
make install")))))
      (h2 "License")
      (p ,license)
      (h2 "Git Repository")
      ,@(let ((url (string-append "https://git.dthompson.us/" repo ".git")))
          `((p ,name " is developed using the Git version control
system. The official repository is hosted at "
               ,(anchor url url) ".")
            (p "To clone the repository, run:")
            (pre "git clone " ,url)))
      (h2 "Community")
      (p "Real-time discussion for " ,name " can be found on the "
         (code ,irc-channel)
         " channel on the "
         ,(anchor "Libera.chat" "https://libera.chat")
         " IRC network")
      (h2 "Contributing")
      (p "Send patches and bug reports to "
         ,(anchor "davet@gnu.org" "mailto:davet@gnu.org")
         ".")))

  (static-page name (string-append "projects/" file-name) body))
