;;; Copyright © 2019 - 2024 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Reader for Org syntax which invokes `org-export' via the Emacs daemon for
;;; rendering and metadata extraction.
;;;
;;; The choice to leverage Emacs, rather than writing a parser in Guile, was
;;; made because it enabled us to leverage other Emacs facilities such as
;;; `font-lock' and `htmlize' for syntax highlighting.
;;;
;;; Code:

(define-module (jakob reader org-mode)
  #:use-module (haunt reader)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (jakob utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi-197)
  #:use-module (sxml simple)
  #:export (render-org-mode-file
            extract-org-mode-metadata
            org-mode-reader))

;; Directory to store cached artifacts in.
;;
;; Caching is disabled if this is `#f'.
(define %cache-directory
  (make-parameter (if (getenv "HAUNT_ORG_READER_DISABLE_CACHE")
                      #f
                      (or (getenv "HAUNT_ORG_READER_CACHE_DIR")
                          "./.org-mode-reader-cache/"))))

;; File name of Elisp script to execute before anything else
(define %additional-emacs-preamble
  (make-parameter (getenv "HAUNT_ORG_READER_EMACS_PREAMBLE")))

;; Additional Org-mode keywords to include in the extracted metadata.
(define %additional-keys
  (make-parameter '("CROSSPOST" "SCRIPTS" "META-TAGS")))

;; Whether to use a running Emacs daemon to evaluate elisp forms.
(define %use-emacsclient
  (make-parameter (getenv "HAUNT_ORG_READER_USE_EMACSCLIENT")))

(define (eval-in-emacs form)
  "Evaluate s-exp FORM in Emacs and return the result

If `%use-emacsclient' is truthy, evaluate FORM in the current running Emacs
daemon. Assumes that FORM does not write to `standard-output'."
  (let* (;; If `HAUNT_ORG_READER_PREAMBLE' is specified, load that.
         (form (if (%additional-emacs-preamble)
                   `(progn
                     (load-file ,(%additional-emacs-preamble))
                     ,form)
                   form))
         ;; We need to explicitly request that Emacs write the result if using
         ;; Emacs batch mode (which is how we evaluate forms without
         ;; `emacsclient'.)
         (form (if (not (%use-emacsclient))
                   `(print ,form)
                   form))
         (stringified (call-with-output-string (cut write form <>)))
         (port       (if (%use-emacsclient)
                         (open-pipe* OPEN_READ "emacsclient" "-e" stringified)
                         (open-pipe* OPEN_READ "emacs" "--batch" "--eval" stringified)))
         (result     (read port))
         ;; The symbol `nil' doesn't have the same semantics in Scheme, so we'll
         ;; convert it to the empty list.
         (result     (if (eqv? result 'nil)
                         '()
                         result)))
    (if (eqv? 0 (status:exit-val (close-pipe port)))
        result
        (error "could not eval" form))))

(define (render-org-mode-file file-name)
  "Export FILE-NAME as an HTML document string"
  (define output-file-name (tmpnam))
  (define result
    (eval-in-emacs
     `(save-excursion
       ;;(load-file "./emacs-htmlize/htmliz.el")
       (setq org-html-htmlize-output-type 'css)
       (let ((enable-local-variables :all))
         (set-buffer (find-file-noselect ,file-name)))
       (setq-local org-export-filter-latex-fragment-functions
                   (list (lambda (data backend channel)
                           (org-html-encode-plain-text data))))
       (let ((result (org-export-as 'html nil nil t)))
         (with-temp-buffer
          (insert result)
          (write-region (point-min) (point-max) ,output-file-name))))))
  (define parsed (call-with-input-file output-file-name get-string-all))
  (delete-file output-file-name)
  ;; We wrap in a `div' because when we call `xml->sxml' later on in
  ;; `read-org-mode-post-fresh', it is expecting a single element.
  (format #f "<div>~a</div>" parsed))

;; for testing to show inline-css color style
(define (render-org-mode-file-fix file-name)
  "Export FILE-NAME as an HTML document string"
  (define output-file-name (tmpnam))
  (define result
    (eval-in-emacs
     `(save-excursion
       (load-file "./emacs-htmlize/ox-haunt.el")
       (setq org-html-htmlize-output-type 'inline-css)
       (let ((enable-local-variables :all))
         (set-buffer (find-file-noselect ,file-name)))
       (setq-local org-export-filter-latex-fragment-functions
                   (list (lambda (data backend channel)
                           (org-html-encode-plain-text data))))
       (ox-haunt-export-as-html nil nil nil t (org-export-get-environment))
       (let ((htmlbuf (get-buffer "*Org Haunt Export*")))
         (with-current-buffer htmlbuf
          (write-region (point-min) (point-max) ,output-file-name))))))
  (define parsed (call-with-input-file output-file-name get-string-all))
  (delete-file output-file-name)
  ;; We wrap in a `div' because when we call `xml->sxml' later on in
  ;; `read-org-mode-post-fresh', it is expecting a single element.
  (format #f "<div>~a</div>" parsed))

(define (extract-org-mode-metadata-raw file-name)
  (map (match-lambda
         ((key value) `(,(string->symbol (string-downcase key)) . ,value)))
       (eval-in-emacs
        `(save-excursion
          (let ((enable-local-variables :all))
            (set-buffer (find-file-noselect ,file-name)))
          (org-collect-keywords ',(append '("TITLE" "DATE" "TAGS")
                                          (%additional-keys)))))))

(define (parse-metadata metadata-alist)
  (chain metadata-alist
         (assq-map! _ 'date (cut string->date <> "<~Y-~m-~d ~a ~H:~M>"))
         (assq-map! _ 'tags (cut string-split <> (char-set #\space #\, #\:)))))

;; This is the public-facing interface. Because dates aren't serializable with
;; `write', the internal interface has extraction and parsing broken out into
;; separate procedures.
(define (extract-org-mode-metadata file-name)
  "Parse the metadata out of FILE-NAME as an alist"
  (chain file-name
         (extract-org-mode-metadata-raw _)
         (parse-metadata _)))

(define (metadata-file-name hash)
  (string-append (%cache-directory)
                 file-name-separator-string
                 hash
                 "-metadata"))
(define (sxml-file-name hash)
  (string-append (%cache-directory)
                 file-name-separator-string
                 hash
                 "-sxml"))

(define (read-org-mode-post-cached hash)
  (values (parse-metadata (call-with-input-file (metadata-file-name hash) read))
          (call-with-input-file (sxml-file-name hash) read)))

(define (read-org-mode-post-fresh hash file-name)
  (let ((metadata (extract-org-mode-metadata-raw file-name))
        (sxml (match (call-with-input-string (render-org-mode-file file-name) xml->sxml)
                (('*TOP* ('div sxml ...)) sxml))))
    (when (%cache-directory)
      (call-with-output-file (metadata-file-name hash) (cut write metadata <>))
      (call-with-output-file (sxml-file-name hash)     (cut write sxml <>)))
    (values (parse-metadata metadata) sxml)))

(define (read-org-mode-post file-name)
  (define hash
    (let* ((port (open-pipe* OPEN_READ "md5sum" file-name))
           (result (string-trim-both (get-string-all port))))
      (unless (eqv? 0 (status:exit-val (close-pipe port)))
        (error "cannot hash file"))
      (first (string-split result #\ ))))
  (when (%cache-directory)
    (cond ((and (file-exists? (%cache-directory))
                (not (eqv? 'directory (stat:type (stat (%cache-directory))))))
           (error "cache directory exists but is not a directory"
                  (%cache-directory)))
          ((not (file-exists? (%cache-directory)))
           (mkdir (%cache-directory)))))
  (if (and (%cache-directory)
           (file-exists? (metadata-file-name hash))
           (file-exists? (sxml-file-name hash)))
      (read-org-mode-post-cached hash)
      (read-org-mode-post-fresh hash file-name)))

(define org-mode-reader
  (make-reader (make-file-extension-matcher "org")
               read-org-mode-post))
