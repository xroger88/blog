(use-modules (gnu packages autotools)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages rsync)
             (gnu packages texinfo)
             (guix git-download)
             (guix packages)
             (guix profiles)
             (guix utils))

(define guile-syntax-highlight*
  (let ((commit "d68ccf7c2ae9516ca2ddacc5f65e2277038b23f6"))
    (package
      (inherit guile-syntax-highlight)
      (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.dthompson.us/guile-syntax-highlight.git")
                    (commit commit)))
              (sha256
               (base32
                "0sbxy7mn6kzx83ml4x530r4g7b22jk1kpp766mcgm35zw7mn1qi9"))))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'bootstrap
             (lambda _ (invoke "sh" "bootstrap"))))))
      (inputs (list guile-3.0-latest))
      (native-inputs (list autoconf automake pkg-config)))))

(define haunt*
  (let ((commit "bdf0ebe0e4e90b14812bccd5bf25d0aeac9ab7b2"))
    (package
     (inherit haunt)
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.dthompson.us/haunt.git")
                    (commit commit)))
              (sha256
               (base32
                "0zwyjwzkpn3a31chgy9nlx274hm6jmdbffycynmhjaa5j8x19ji6"))))
     (native-inputs
      (list automake autoconf pkg-config texinfo))
     (inputs
      (list rsync guile-3.0-latest))
     (arguments
      (substitute-keyword-arguments (package-arguments haunt)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'bootstrap
              (lambda _
                (invoke "sh" "bootstrap"))))))))))

(packages->manifest
 (list guile-3.0-latest
       guile-syntax-highlight*
       haunt*))
