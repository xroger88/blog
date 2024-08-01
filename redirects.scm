;;; Copyright © 2023 David Thompson <davet@gnu.org>
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

(define-module (redirects)
  #:export (redirects))

;; All of my posts used to live in the site root, but I later moved
;; them to /posts and changed the slug generator.  These redirects
;; keep the original URLs alive.
(define redirects
  '(("/a-cooperative-repl-server-for-guile-2010.html" "/posts/a-cooperative-repl-server-for-guile-2-0-10.html")
    ("/angularjs-post-mortem.html" "/posts/angularjs-post-mortem.html")
    ("/catbird-an-experimental-game-engine-for-scheme-programmers.html" "/posts/catbird-an-experimental-game-engine-for-scheme-programmers.html")
    ("/chickadee-0100-released.html" "/posts/chickadee-0-10-0-released.html")
    ("/chickadee-030-released.html" "/posts/chickadee-0-3-0-released.html")
    ("/chickadee-040-released.html" "/posts/chickadee-0-4-0-released.html")
    ("/chickadee-050-released.html" "/posts/chickadee-0-5-0-released.html")
    ("/chickadee-060-released.html" "/posts/chickadee-0-6-0-released.html")
    ("/chickadee-080-released.html" "/posts/chickadee-0-8-0-released.html")
    ("/chickadee-090-released.html" "/posts/chickadee-0-9-0-released.html")
    ("/find-me-on-diaspora.html" "/posts/find-me-on-diaspora.html")
    ("/first-gnu-guile-patch-and-more-guix-packages.html" "/posts/first-gnu-guile-patch-and-more-guix-packages.html")
    ("/font-rendering-in-opengl-with-pango-and-cairo.html" "/posts/font-rendering-in-opengl-with-pango-and-cairo.html")
    ("/functional-reactive-programming-in-scheme-with-guile-2d.html" "/posts/functional-reactive-programming-in-scheme-with-guile-2d.html")
    ("/gnu-30th-anniversary-hackathon.html" "/posts/gnu-30th-anniversary-hackathon.html")
    ("/guile-2d---a-2d-game-development-framework-for-gnu-guile.html" "/posts/guile-2d-a-2d-game-development-framework-for-gnu-guile.html")
    ("/guile-2d-01-release.html" "/posts/guile-2d-0-1-release.html")
    ("/guile-2d-is-now-named-sly.html" "/posts/guile-2d-is-now-named-sly.html")
    ("/guile-sdl2-031-released.html" "/posts/guile-sdl2-0-3-1-released.html")
    ("/guile-sdl2-040-released.html" "/posts/guile-sdl2-0-4-0-released.html")
    ("/guile-sdl2-050-released.html" "/posts/guile-sdl2-0-5-0-released.html")
    ("/guile-sdl2-070-released.html" "/posts/guile-sdl2-0-7-0-released.html")
    ("/guile-sdl2-080-released.html" "/posts/guile-sdl2-0-8-0-released.html")
    ("/guile-syntax-highlight-020-released.html" "/posts/guile-syntax-highlight-0-2-0-released.html")
    ("/guile-websocket-01-released.html" "/posts/guile-websocket-0-1-released.html")
    ("/guix-for-development.html" "/posts/guix-for-development.html")
    ("/haunt-02-released.html" "/posts/haunt-0-2-released.html")
    ("/haunt-021-released.html" "/posts/haunt-0-2-1-released.html")
    ("/haunt-022-released.html" "/posts/haunt-0-2-2-released.html")
    ("/haunt-023-released.html" "/posts/haunt-0-2-3-released.html")
    ("/haunt-024-released.html" "/posts/haunt-0-2-4-released.html")
    ("/haunt-026-released.html" "/posts/haunt-0-2-6-released.html")
    ("/hello-world.html" "/posts/hello-world.html")
    ("/how-to-apply-hilltops-boku-no-natsuyasumi-2-english-patch-on-linux.html" "/posts/boku-no-natsuyasumi-2-english-patch-linux")
    ("/i-will-be-presenting-about-gnu-guix-at-libreplanet-2018.html" "/posts/i-will-be-presenting-about-gnu-guix-at-libreplanet-2018.html")
    ("/installing-guix-on-a-10th-gen-thinkpad-x1.html" "/posts/installing-guix-on-a-10th-gen-thinkpad-x1.html")
    ("/introducing-credsummoner-a-lightweight-tool-for-generating-temporary-aws-credentials.html" "/posts/introducing-credsummoner-a-lightweight-tool-for-generating-temporary-aws-credentials.html")
    ("/introducing-haunt.html" "/posts/introducing-haunt.html")
    ("/issues-with-object-oriented-programming-in-guile.html" "/posts/issues-with-object-oriented-programming-in-guile.html")
    ("/jump-to-jasmine-specs-with-rinari.html" "/posts/jump-to-jasmine-specs-with-rinari.html")
    ("/liberating-a-thinkpad-x220.html" "/posts/liberating-a-thinkpad-x220.html")
    ("/live-asset-reloading-with-guile-2d.html" "/posts/live-asset-reloading-with-guile-2d.html")
    ("/maine.html" "/posts/maine.html")
    ("/my-first-gnu-guix-patch.html" "/posts/my-first-gnu-guix-patch.html")
    ("/my-first-real-foss-contribution.html" "/posts/my-first-real-foss-contribution.html")
    ("/rendering-html-with-sxml-and-gnu-guile.html" "/posts/rendering-html-with-sxml-and-gnu-guile.html")
    ("/reproducible-development-environments-with-gnu-guix.html" "/posts/reproducible-development-environments-with-gnu-guix.html")
    ("/ruby-on-guix.html" "/posts/ruby-on-guix.html")
    ("/spring-lisp-game-jam-2023-summary.html" "/posts/spring-lisp-game-jam-2023-summary.html")
    ("/stumpwm-on-debian-wheezy.html" "/posts/stumpwm-on-debian-wheezy.html")
    ("/syncing-required-packages-in-emacs.html" "/posts/syncing-required-packages-in-emacs.html")
    ("/the-little-schemer.html" "/posts/the-little-schemer.html")))
