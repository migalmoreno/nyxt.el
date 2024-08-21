(use-modules (gnu packages emacs-xyz)
             (gnu packages web-browsers)
             (guix build utils)
             (guix build-system emacs)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (ice-9 popen)
             (ice-9 rdelim))

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
    (home-page "https://git.migalmoreno.com/nyxt.el")
    (synopsis "Interact with Nyxt from Emacs")
    (description "This package consists of custom logic to interact with Nyxt
from Emacs.")
    (license license:gpl3+)))

emacs-nyxt
