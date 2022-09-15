(define-module (emacs-nyxt)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix git-download)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

(define %source-dir (dirname (current-filename)))

(define (emacs-nyxt-git-version)
  (let* ((port (with-directory-excursion
                %source-dir
                (open-input-pipe "git describe --always --tags")))
         (version (read-line port)))
    (close-pipe port)
    version))

(define-public emacs-nyxt
  (package
    (name "emacs-nyxt")
    (version (emacs-nyxt-git-version))
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (git-predicate %source-dir)))
    (build-system emacs-build-system)
    (inputs
     (list nyxt))
    (propagated-inputs
     (list emacs-sly))
    (home-page "https://git.sr.ht/~conses/nyxt.el")
    (synopsis "Helper functions to interact with Nyxt.")
    (description "nyxt.el consists of many helper functions to interact with Nyxt from Emacs.")
    (license license:gpl3+)))

emacs-nyxt
