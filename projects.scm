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

(define-module (projects)
  #:use-module (highlight)
  #:use-module (theme)
  #:use-module (utils)
  #:export (chickadee-page
            sly-page
            guile-sdl2-page
            guile-syntax-highlight-page
            guile-websocket-page
            haunt-page
            shroud-page
            srt2vtt-page))

(define sly-page
  (project-page
   #:name "Sly"
   #:file-name "sly.html"
   #:description
   `((p (strong "note: this project is no longer being developed!"))
     (p "Sly is a fun, free software 2D/3D game engine written in "
        ,(anchor "Guile Scheme" "https://gnu.org/s/guile") ".")
     ,(centered-image "/images/sly/logo.png" "Sly fox mascot")
     (p "Sly differentiates itself from most other game engines by encouraging
and enabling the use of "
        ,(anchor "live coding"
                 "http://toplap.org/about/")
        " and "
        ,(anchor "functional reactive programming"
                 "https://en.wikipedia.org/wiki/Functional_reactive_programming")
        " techniques.  Sly provides a dynamic live coding environment
that allows games to be built interactively and iteratively without
ever stopping the running program.  A data structure called a “signal”
provides a method of modeling time-varying state that is declarative,
functional, and reactive.")
     (p ,(centered-image "/images/sly/2048.png"))
     (p ,(centered-image "/images/sly/mines.png")))
   #:requirements '("GNU Guile >= 2.0.11"
                    "guile-opengl_ >= 0.1.0"
                    "guile-sdl_ >= 0.5.0"
                    "SDL 1.2.x"
                    "FreeImage >= 3.0"
                    "GNU Scientific Library (GSL)")
   #:license "GNU GPLv3+"
   #:repo "sly"
   #:manual? #t
   #:releases
   `(("0.1" ,(date 2015 11 12)))))

(define guile-sdl2-page
  (project-page
   #:name "Guile-SDL2"
   #:file-name "guile-sdl2.html"
   #:repo "guile-sdl2"
   #:guix-package "guile-sdl2"
   #:description
   `((p "Guile-SDL2 provides "
        ,(anchor "Guile Scheme" "https://gnu.org/s/guile")
        " bindings for the "
        ,(anchor "SDL2" "http://libsdl.org")
        " C shared library.  The bindings are written in pure Scheme
using Guile's foreign function interface."))
   #:usage
   `((p "Guile-SDL2 provides modules in the "
        (code "(sdl2 ...)")
        " namespace, roughly organized how the SDL2 C header files are
organized.  Low-level bindings are available in the"
        (code "(sdl2 bindings ...)")
        " namespace, but these are not recommended for normal usage.")
     (p "Additionally, SDL2 extension library bindings are available in the
following modules:")
     (ul (li "SDL2_image: " (code "(sdl2 image)"))
         (li "SDL2_mixer: " (code "(sdl2 mixer)"))
         (li "SDL2_ttf: " (code "(sdl2 ttf)")))
     (p "Here is a short “hello, world” example program:")
     ,(highlight-scheme
       "(use-modules (sdl2)
             (sdl2 render)
             (sdl2 surface)
             (sdl2 video))

(define (draw ren)
  (let* ((surface (load-bmp \"hello.bmp\"))
         (texture (surface->texture ren surface)))
    (clear-renderer ren)
    (render-copy ren texture)
    (present-renderer ren)
    (sleep 2)))

(sdl-init)

(call-with-window (make-window)
  (lambda (window)
    (call-with-renderer (make-renderer window) draw)))

(sdl-quit)"))
   #:requirements '("GNU Guile >= 2.0.9"
                    "SDL2 >= 2.0.0"
                    "SDL2_image >= 2.0.0"
                    "SDL2_mixer >= 2.0.0"
                    "SDL2_ttf >= 2.0.0")
   #:license "GNU LGPLv3+"
   #:manual? #t
   #:releases
   `(("0.8.0" ,(date 2022 10 23))
     ("0.7.0" ,(date 2021 10 07))
     ("0.6.0" ,(date 2021 04 13))
     ("0.5.0" ,(date 2020 04 07))
     ("0.4.0" ,(date 2019 06 02))
     ("0.3.1" ,(date 2018 10 16))
     ("0.3.0" ,(date 2018 07 10))
     ("0.2.0" ,(date 2017 01 20))
     ("0.1.2" ,(date 2016 08 10))
     ("0.1.1" ,(date 2016 01 01))
     ("0.1.0" ,(date 2015 12 22)))))

(define guile-syntax-highlight-page
  (project-page
   #:name "guile-syntax-highlight"
   #:file-name "guile-syntax-highlight.html"
   #:repo "guile-syntax-highlight"
   #:description
   `((p "guile-syntax-highlight is a general-purpose syntax
highlighting library for GNU Guile.  It can parse code written in
various programming languages into a simple s-expression that can be
easily converted to HTML (via SXML) or any other format for
rendering.")
     (p "The following languages/formats are currently supported:"
        (ul
         ,@(map (lambda (lang) `(li ,lang))
                '("C" "Common Lisp" "CSS" "gitignore" "Scheme" "XML")))))
   #:usage
   (highlight-scheme
    "(use-modules (syntax-highlight)
             (syntax-highlight scheme)
             (sxml simple))

(define code
  \"(define (square x) \\\"Return the square of X.\\\" (* x x))\")

;; Get raw highlights list.
(define highlighted-code
  (highlight lex-scheme code))

;; Convert to SXML.
(define highlighted-sxml
  (highlights->sxml highlighted-code))

;; Write HTML to stdout.
(sxml->xml highlighted-sxml)
(newline)
")
   #:requirements '("GNU Guile >= 2.0"
                    "GNU Make"
                    "GNU pkg-config")
   #:license "GNU LGPLv3+"
   #:releases
   `(("0.2.0" ,(date 2022 12 11))
     ("0.1" ,(date 2018 03 10)))))

(define guile-websocket-page
  (project-page
   #:name "guile-websocket"
   #:file-name "guile-websocket.html"
   #:repo "guile-websocket"
   #:description
   `((p "guile-websocket provides an implementation of the WebSocket protocol
as defined by "
        (a (@ (href "https://www.rfc-editor.org/rfc/rfc6455")) "RFC 6455")
        "."))
   #:usage
   `(p
     "Below is a simple example program that demonstrates basic usage of
the guile-websocket API:"
     ,(highlight-scheme
       "(use-modules (web socket server))

;; Respond to text messages by reversing the message.  Respond to
;; binary messages with \"hello\".
(define (handler data)
  (if (string? data)
      (string-reverse data)
      \"hello\"))

(run-server handler (make-server-socket #:port 9090))
"))
   #:requirements '("GNU Guile >= 2.0"
                    "GNU Make"
                    "GNU pkg-config")
   #:license "GNU LGPLv3+"
   #:releases
   `(("0.1" ,(date 2022 11 12)))))

(define haunt-page
  (project-page
   #:name "Haunt"
   #:file-name "haunt.html"
   #:repo "haunt"
   #:guix-package "haunt"
   #:manual? #t
   #:description
   `((p "Haunt is a simple, functional, hackable static site generator
that gives authors the ability to treat websites as Scheme programs.")
     ,(centered-image "/images/haunt/logo.png" "crudely drawn ghost")
     (p "By giving authors the full expressive power of Scheme, they
are able to control every aspect of the site generation process.
Haunt provides a simple, functional build system that can be easily
extended for this purpose.")
     (p "Haunt has no opinion about what markup language authors
should use to write posts, though it comes with support for the
popular Markdown format.  Likewise, Haunt has no opinion about how
authors structure their sites.  Though it comes with support for
building simple blogs or Atom feeds, authors should feel empowered to
tweak, replace, or create builders to do things that aren't provided
out-of-the-box."))
   #:usage
   `((p "Here's what a simple Haunt configuration looks like:")
     ,(call-with-input-file "snippets/haunt.scm" highlight-scheme)
     (p "In a new directory, save the above to a file named "
        (code "haunt.scm") ".")
     (p "Create subdirectories named " (code "posts") " and " (code "images")
        ".")
     (p "Add the following to a new file named " (code "posts/hello.md") ":")
     ,(call-with-input-file "snippets/haunt-hello.md" raw-snippet)
     (p "Run " (code "haunt build") " to build the site.")
     (p "Run " (code "haunt serve") " to a launch a web server to
preview your work.")
     (p "Open " ,(anchor "http://localhost:8080") " in your web browser
and smile, because you've just generated your first Haunt site!")
     (p "Check out other sites built with Haunt over at "
        ,(anchor "awesome.haunt.page" "https://awesome.haunt.page/")
        "!"))
   #:requirements
   '("GNU Guile >= 2.0"
     "guile-commonmark (for Markdown support, optional)"
     "guile-reader (for Skribe support, optional)")
   #:license "GNU GPLv3+"
   #:releases
   `(("0.3.0" ,(date 2024 02 19))
     ("0.2.6" ,(date 2022 01 26))
     ("0.2.5" ,(date 2021 04 15))
     ("0.2.4" ,(date 2018 11 29))
     ("0.2.3" ,(date 2018 11 25))
     ("0.2.2" ,(date 2018 03 10))
     ("0.2.1" ,(date 2017 01 23))
     ("0.2"   ,(date 2016 04 24))
     ("0.1"   ,(date 2015 08 08)))))

(define chickadee-page
  (project-page
   #:name "Chickadee"
   #:file-name "chickadee.html"
   #:repo "chickadee"
   #:guix-package "guile-chickadee"
   #:manual? #t
   #:irc-channel "#chickadee"
   #:description
   `((p "Chickadee is a game development toolkit for "
        ,(anchor "Guile Scheme" "https://gnu.org/s/guile") ".")
     ,(centered-image "/images/chickadee/logo.png" "Chickadee logo")
     (p "Chickadee provides all the essential tools that
parenthetically inclined game developers need to make games in
Scheme.")
     (p "Here is the obligatory “Hello, world” program:")
     ,(highlight-scheme
       "(define (draw alpha)
  (draw-text \"Hello, world!\" (vec2 260.0 240.0)))
")
     ,(centered-image "/images/chickadee/screenshot-hello-world.png"
                      "Hello world example screenshot")
     (p "And here's how to draw a sprite:")
     ,(highlight-scheme
       "(use-modules (chickadee graphics sprite))

(define sprite (load-image \"chickadee.png\"))

(define (draw alpha)
  (draw-sprite sprite (vec2 256.0 176.0)))
")
     ,(centered-image "/images/chickadee/screenshot-sprite.png"
                      "Sprite example screenshot")
     (p "Chickadee can render vector paths:")
     ,(highlight-scheme
       "(use-modules (chickadee graphics path))

(define gradient
  (radial-gradient #:start-color tango-light-sky-blue
                   #:end-color tango-dark-sky-blue
                   #:radius 240.0
                   #:origin (vec2 240.0 240.0)))

(define painter
  (let ((filled-circle (with-style ((fill-color gradient))
                         (fill (circle (vec2 240.0 240.0) 240.0)))))
    (translate (vec2 80.0 0.0) (corner-split filled-circle 4))))

(define canvas (make-canvas painter))

(define (draw alpha)
  (draw-canvas canvas))
")
     ,(centered-image "/images/chickadee/screenshot-vector-path.png"
                      "Vector path example screenshot")
     (p "And it can also render 3D models:")
     ,(highlight-scheme
       "(use-modules (chickadee graphics light)
             (chickadee graphics model)
             (chickadee graphics skybox))

(define model (load-gltf \"Suzanne.gltf\"))
(define camera-position (vec3 0.0 0.0 3.0))
(define world (make-identity-matrix4))
(define view (look-at camera-position (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0)))
(define projection (perspective-projection (/ pi 3.0) (/ 4.0 3.0) 0.1 5.0))
(define skybox
  (make-skybox
   (load-cube-map #:right \"right.jpg\"
                  #:left \"left.jpg\"
                  #:top \"top.jpg\"
                  #:bottom \"bottom.jpg\"
                  #:front \"front.jpg\"
                  #:back \"back.jpg\")))

(define (draw alpha)
  (with-projection projection
    (draw-skybox skybox view)
    (draw-model model
                #:model-matrix world
                #:view-matrix view
                #:camera-position camera-position
                #:skybox skybox)))
")
     ,(centered-image "/images/chickadee/screenshot-3d.png"
                      "3D example screenshot")
     (p "Other features include:")
     (ul
      (li "extensible, fixed-timestep game loop")
      (li "keyboard, mouse, and controller input events")
      (li "vectors, matrices, bounding boxes, and easing functions")
      (li "audio playback")
      (li "asynchronous scripting"))
     (p (small ,(anchor "chickadee sprite by Refuzzle, CC0"
                        "http://opengameart.org/content/winter-birds"))))
   #:requirements '("GNU Guile >= 3.0.6"
                    "Guile-SDL2 >= 0.8.0"
                    "Guile-OpenGL >= 0.1.0"
                    "libjpeg-turbo"
                    "libpng"
                    "OpenAL"
                    "Vorbis"
                    "MPG123"
                    "Freetype"
                    "GNU Readline")
   #:license "GNU GPLv3+"
   #:releases
   `(("0.10.0" ,(date 2023 05 24))
     ("0.9.0" ,(date 2022 10 25))
     ("0.8.0" ,(date 2021 10 07))
     ("0.7.0" ,(date 2021 04 13))
     ("0.6.0" ,(date 2020 11 19))
     ("0.5.0" ,(date 2020 04 08))
     ("0.4.0" ,(date 2019 06 04))
     ("0.3.0" ,(date 2018 10 03))
     ("0.2.0" ,(date 2017 01 26))
     ("0.1.0" ,(date 2017 01 23)))))

(define shroud-page
  (project-page
   #:name "Shroud"
   #:file-name "shroud.html"
   #:repo "shroud"
   #:description
   `((p "Shroud is a simple secret manager with a command line interface.")
     (p "The password database is stored as a Scheme s-expression
and encrypted with a "
        ,(anchor "GnuPG" "https://gnupg.org")
        " key.  Secrets consist of an arbitrary number of key/value
pairs, making Shroud suitable for more than just password storage.
For copying and pasting secrets into web browsers and other graphical
applications, there is xclip integration."))
   #:requirements '("GNU Guile >= 2.0.9"
                    "GnuPG >= 1.4"
                    "GNU Make"
                    "GNU pkg-config"
                    ("optional: xclip is needed for the "
                     (code "-c")
                     " flag of "
                     (code "shroud show")
                     " to work"))
   #:usage
   `((p "First, create a " (code ".shroud")
        " file in your home directory to hold your
configuration settings.  All you really need to set here is your GPG
user ID i.e. your email address:")
     ,(highlight-scheme
       "'((user-id . \"foo@example.com\"))")
     (p "The "
        (code ".shroud")
        " file is Scheme source code, so any expression that evaluates
to an alist of valid configuration settings is usable by Shroud.")
     (p "Once Shroud is configured, try out the following commands to
get a feel for how things work:")
     (pre
      "# Add a new secret:
shroud hide bank-account username=foobar password=hackme

# Edit an existing secret:
shroud hide --edit bank-account password=hackmepls

# List all secrets:
shroud list

# Show all key/value pairs for a saved secret:
shroud show bank-account

# Show a single value in a secret:
shroud show bank-account password

# Copy a password directly to X clipboard:
shroud show -c bank-account password

# Delete a secret:
shroud remove bank-account")
     (p "Happy shrouding!"))
   #:license "GNU GPLv3+"
   #:releases
   `(("0.1.1" ,(date 2015 10 01))
     ("0.1.0" ,(date 2015 09 29)))))

(define srt2vtt-page
  (project-page
   #:name "srt2vtt"
   #:file-name "srt2vtt.html"
   #:repo "srt2vtt"
   #:description
   `((p "Convert SRT formatted subtitles to WebVTT format for use with
the HTML5 "
        (code "<track>") " tag."))
   #:requirements '("GNU Guile >= 2.0.5")
   #:usage
   `((pre
      "$ srt2vtt --help
Usage: srt2vtt [OPTIONS]
Convert SubRip formatted subtitles to WebVTT format.

  -h, --help             display this help and exit
  -v, --version          display version and exit
  -i, --input=FILE-NAME  read input from FILE-NAME
  -o, --output=FILE-NAME write output to FILE-NAME")
     (p "If " (code "--input")
        " or " (code "--output")
        " is ommitted, read from stdin or stdout, respectively."))
   #:license "GNU GPLv3+"
   #:releases
   `(("0.2" ,(date 2021 03 19))
     ("0.1" ,(date 2015 02 7)))))
