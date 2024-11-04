;;; nyxt.el --- Emacs integration with Nyxt  -*- lexical-binding: t; -*-

;; Copyright © 2022, 2023, 2024 Miguel Ángel Moreno <mail@migalmoreno.com>

;; Author: Miguel Ángel Moreno <mail@migalmoreno.com>
;; Version: 0.2.0
;; Keywords: tools, processes
;; URL: https://git.migalmoreno.com/nyxt.el
;; Package-Requires: ((emacs "25.1") sly)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides functions to interact with a Nyxt process.

;;; Code:

(eval-and-compile
  (require 'cl-lib)
  (require 'sly)
  (sly-setup))

(defgroup nyxt nil
  "Nyxt browser integrations and tweaks."
  :group 'external)

(defcustom nyxt-autostart-delay 0
  "Seconds to delay the evaluation of expressions for upon Nyxt startup."
  :type 'integer
  :group 'nyxt)

(defcustom nyxt-port 4006
  "Default port to use for the Slynk connection to Nyxt."
  :type 'integer
  :group 'nyxt)

(defcustom nyxt-path (executable-find "nyxt")
  "The system path to the Nyxt executable."
  :type 'string
  :group 'nyxt)

(defcustom nyxt-startup-flags '("-e" "(start-slynk)")
  "Nyxt flags to use to start the Nyxt process."
  :type '(repeat string)
  :group 'nyxt)

(defvar nyxt-process nil
  "The current Nyxt process.")

(defvar nyxt-sly-connection nil
  "The current Sly connection for communicating with Nyxt.")

;;;###autoload (autoload 'nyxt-map "nyxt" "Map to bind `nyxt' commands to." nil 'keymap)
(defvar nyxt-map nil
  "Map to bind `nyxt' commands to.")

(defvar nyxt-autostart-p nil)

(defun nyxt--sly-connected-p ()
  "Indicate whether there's currently a connection to `nyxt-port'."
  (cl-find-if (lambda (p)
                (= (sly-connection-port p) nyxt-port))
              sly-net-processes))

(cl-defun nyxt--sly-eval (sexps &rest args &key &allow-other-keys)
  "Evaluate SEXPS and ARGS in the current Nyxt Sly connection."
  (let ((sexp (if (cl-every #'consp sexps)
                  (mapconcat #'prin1-to-string sexps "")
                (prin1-to-string sexps)))
        (sly-dispatching-connection (if (nyxt--sly-connected-p)
                                        nyxt-sly-connection
                                      (nyxt-sly-connect))))
    (cl-flet ((eval-sexp ()
                (let ((sly-dispatching-connection nyxt-sly-connection))
                  (apply #'sly-eval
                         `(slynk:interactive-eval-region ,sexp) args))))
      (while (not (sly-mrepl--find-buffer sly-dispatching-connection))
        (sleep-for 0.1))
      (with-current-buffer (sly-mrepl--find-buffer sly-dispatching-connection)
        (while (not sly-mrepl--local-channel)
          (sleep-for 0.1))
        (while (not (sly-current-package))
          (sleep-for 0.1))
        (unless (string= (sly-current-package) "nyxt-user")
          (sly-mrepl--eval-for-repl
           '(slynk-mrepl:guess-and-set-package "nyxt-user")))
        (if nyxt-autostart-p
            (progn
              (run-at-time nyxt-autostart-delay nil #'eval-sexp)
              (setq nyxt-autostart-p nil))
          (eval-sexp))))))

(defun nyxt--system-process-p ()
  "Return non-nil if the Nyxt system process is currently running."
  (cl-some (lambda (pid)
             (string-match (rx (* any) "nyxt" (* any))
                           (assoc-default 'comm (process-attributes pid))))
           (list-system-processes)))

(cl-defun nyxt-run (sexps &key (autostart nil))
  "Evaluate SEXPS in the context of the current Nyxt connection.

If AUTOSTART is non-nil and a Nyxt system process is not found it will
automatically create a Sly connection for it."
  (let* ((sly-log-events nil))
    (cond
     ((and (not (nyxt--system-process-p))
           (not (nyxt--sly-connected-p))
           (not nyxt-process)
           autostart)
      (message "Launching Nyxt...")
      (setq nyxt-process
            (apply #'start-process "nyxt" nil nyxt-path nyxt-startup-flags))
      (set-process-filter
       nyxt-process
       (lambda (process output)
         (with-temp-buffer
           (goto-char (point-max))
           (forward-line 0)
           (cond
            ((string-match (rx (+ any) "Slynk server started at port") output)
             (while (or (not (sly-current-connection))
                        (not (sly-connected-p))
                        (not (nyxt--sly-connected-p))
                        (not nyxt-sly-connection))
               (nyxt-sly-connect)
               (sleep-for 0.1))
             (setq nyxt-autostart-p t)
             (nyxt--sly-eval sexps))
            ((or (string-match (rx (+ any) "Deleting socket") output)
                 (/= (process-exit-status process) 0))
             (setq nyxt-process nil)
             (setq nyxt-sly-connection nil)))))))
     ((or (nyxt--system-process-p) nyxt-process)
      (nyxt--sly-eval sexps)))))

(defun nyxt--extension-p (system &optional symbol)
  "Check if Nyxt extension SYSTEM exists in the ASDF source registry.
Optionally test if the extension's SYMBOL is bound."
  (when-let ((sys (nyxt--sly-eval `(asdf:find-system ,system nil))))
    (if symbol
        (when-let ((sym (nyxt--sly-eval
                         `(find-symbol ,(upcase symbol)
                                       ,(sly-keywordify (intern system))))))
          (not (string-match "NIL" sym)))
      (not (string= (downcase sys) "nil")))))

;;;###autoload
(defun nyxt-sly-connect ()
  "Connect to a Slynk server via Sly to interact with the Nyxt browser."
  (interactive)
  (setq nyxt-sly-connection (sly-connect "localhost" nyxt-port)))

;;;###autoload
(defun nyxt-init ()
  "Start Nyxt and focus on its window."
  (interactive)
  (nyxt-run nil :autostart t))

;;;###autoload
(defun nyxt-quit ()
  "Quit the Nyxt process."
  (interactive)
  (when nyxt-process
    (ignore-errors
      (kill-process nyxt-process))
    (setq nyxt-process nil)
    (setq nyxt-sly-connection nil)))

;;;###autoload
(defun nyxt-search (query)
  "Search for QUERY in the default search engine in Nyxt."
  (interactive "sSearch for: ")
  (nyxt-run
   `(buffer-load
     (first (nyxt::input->queries ,query))
     :buffer (make-buffer-focus))
   :autostart t))

;;;###autoload
(defun nyxt-load-theme (theme)
  "Load THEME in Nyxt.
THEME can be the name of one of the currently loaded `tailor:user-theme' themes
or a new quoted `tailor:user-theme' instance."
  (interactive
   (list
    (intern
     (completing-read
      "Load theme: "
      (mapcar
       (lambda (theme)
         (intern (downcase (symbol-name theme))))
       (read
        (nyxt--sly-eval
         '(mapcar #'tailor::name
                  (tailor:themes (tailor::current-tailor-mode))))))))))
  (if (nyxt--extension-p "nx-tailor" "load-theme")
      (nyxt-run (if (listp theme)
                    `(nx-tailor:load-theme ,theme)
                  `(nx-tailor:load-theme ',theme)))
    (error "You need the nx-tailor extension to change Nyxt theme")))

;;;###autoload
(defun nyxt-copy-url ()
  "Kill current page URL in Nyxt."
  (interactive)
  (nyxt-run '(copy-url)))

;;;###autoload
(defun nyxt-delete-current-buffer ()
  "Delete the currently-selected buffer in Nyxt."
  (interactive)
  (nyxt-run '(delete-current-buffer)))

;;;###autoload
(defun nyxt-scroll-other-window ()
  "Scroll the Nyxt window."
  (interactive)
  (nyxt-run
   '(nyxt/mode/document::scroll-down)))

;;;###autoload
(defun nyxt-scroll-other-window-down ()
  "Scroll the Nyxt window upward."
  (interactive)
  (nyxt-run
   '(nyxt/mode/document::scroll-up)))

;;;###autoload
(defun nyxt-insert-url ()
  "Insert the current Nyxt buffer's URL into the current buffer."
  (interactive)
  (insert (read (nyxt--sly-eval '(render-url (url (current-buffer)))))))

(define-prefix-command 'nyxt-map)
(let ((map nyxt-map))
  (define-key map "y" #'nyxt-sly-connect)
  (define-key map "i" #'nyxt-init)
  (define-key map "y" #'nyxt-insert-url)
  (define-key map "t" #'nyxt-load-theme)
  (define-key map "q" #'nyxt-quit)
  (define-key map "s" #'nyxt-search)
  (define-key map "w" #'nyxt-copy-url)
  (define-key map "k" #'nyxt-delete-current-buffer)
  (define-key map "v" #'nyxt-scroll-other-window)
  (define-key map "V" #'nyxt-scroll-other-window-down)
  (when (>= emacs-major-version 28)
    (put #'nyxt-scroll-other-window 'repeat-map 'nyxt-map)
    (put #'nyxt-scroll-other-window-down 'repeat-map 'nyxt-map)))

(provide 'nyxt)
;;; nyxt.el ends here
