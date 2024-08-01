
(define-module (reader)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (sxml simple)
  #:use-module (haunt post)
  #:use-module (haunt utils)
  #:use-module (haunt reader)
  #:export (org-reader))


(define ox-haunt-base-dir "~/Projects/GuixDev/blog")
(define emacs-cmd-load "~/.emacs.d/elpa/ox-haunt-20230725.1/ox-haunt.el")
(define emacs-cmd-eval
  (format #f "(progn (require 'ox-haunt) (setq ox-haunt-base-dir ~s))" ox-haunt-base-dir))

(define (org2html-emacs-cmd filepath)
  (format #f "emacs --batch --load ~s --eval ~s ~a --funcall ox-haunt-export-to-html"
          emacs-cmd-load emacs-cmd-eval filepath))

(define (read-org-post port)
  ;; convert org file to html file via emacs ox-haunt
  (let* ((org-file-name (port-filename port))
         (org-file-path (canonicalize-path org-file-name))
         (org-file-dirname (dirname org-file-path))
         (org-file-basename (basename org-file-name ".org"))
         (html-file-path (string-append org-file-dirname
                                        file-name-separator-string
                                        org-file-basename ".html")))
    (system (org2html-emacs-cmd org-file-path))
    ;;(call-with-input-file html-file-path (@@ (haunt reader) read-html-post))
    (call-with-values
        (lambda () (call-with-input-file html-file-path (@@ (haunt reader) read-html-post)))
      (lambda (meta-info sxml-data)
        ;; delete html-suffixed temporary file
        (delete-file html-file-path)
        ;; return the result of read-html-port
        (values meta-info sxml-data)))
    ))

(define org-reader
  (make-reader (make-file-extension-matcher "org")
               (cut call-with-input-file <> read-org-post)))
