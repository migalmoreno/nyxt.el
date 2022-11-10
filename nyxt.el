;;; nyxt.el --- Emacs integration with Nyxt  -*- lexical-binding: t; -*-

;; Copyright ©2022 conses <contact@conses.eu>

;; Author: conses <contact@conses.eu>
;; Version: 0.1.0
;; Keywords: tools, processes
;; URL: https://git.sr.ht/~conses/nyxt.el
;; Package-Requires: ((emacs "25.1") sly cl-lib)

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

(require 'sly)
(eval-when-compile
  (require 'cl-lib))

(defgroup nyxt nil
  "Nyxt browser integrations and tweaks."
  :group 'external)

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

(defvar nyxt-map nil
  "Map to bind `nyxt' commands to.")

(defun nyxt--sly-connected-p ()
  "Indicate whether there's currently a connection to `nyxt-port'."
  (cl-find-if (lambda (p)
                (= (sly-connection-port p) nyxt-port))
              sly-net-processes))

(cl-defun nyxt--sly-eval (sexps &rest args &key &allow-other-keys)
  "Evaluate SEXPS and ARGS in the current Nyxt Sly connection."
  (when-let ((sexp (if (every #'consp sexps)
                       (mapconcat #'prin1-to-string sexps "")
                     (prin1-to-string sexps)))
             (sly-buffer (sly-mrepl--find-buffer nyxt-sly-connection)))
    (with-current-buffer sly-buffer
      (unless (string= (sly-current-package) "nyxt-user")
        (if (plist-get args :startup)
            (cl-remf args :startup)
            (sly-mrepl--eval-for-repl
             '(slynk-mrepl:guess-and-set-package "nyxt-user"))))
      (if (plist-get args :return)
          (car (apply #'sly-eval `(slynk::eval-region ,sexp) (map-delete args :return)))
        (apply #'sly-eval `(slynk:interactive-eval-region ,sexp) args)))))

(defun nyxt--system-process-p ()
  "Return non-nil if the Nyxt system process is currently running."
  (cl-some (lambda (pid)
             (string-match (rx (* any) "nyxt" (* any))
                           (assoc-default 'comm (process-attributes pid))))
           (list-system-processes)))

(defun nyxt--exwm-focus-window ()
  "Handle Nyxt's EXWM window.

Switch to the corresponding EXWM workspace and if the Nyxt window is visible,
focus on it, otherwise switch to its underlying buffer."
  (when (require 'exwm nil t)
    (when-let* ((nyxt-buffer
                 (car (cl-remove-if-not
                       (lambda (buffer)
                         (string-match "Nyxt:" (buffer-name buffer)))
                       (buffer-list))))
                (exwm-workspace
                 (window-frame (or (get-buffer-window nyxt-buffer t)
                                   (with-current-buffer nyxt-buffer
                                     (set-window-buffer
                                      (frame-selected-window exwm--frame)
                                      (current-buffer))
                                     (get-buffer-window (current-buffer) t))))))
      (exwm-workspace-switch (exwm-workspace--position exwm-workspace))
      (if (and (= (exwm-workspace--position exwm-workspace--current)
                  (exwm-workspace--position exwm-workspace))
               (> (length (window-list exwm-workspace)) 1))
          (if (get-buffer-window nyxt-buffer)
              (select-window (get-buffer-window nyxt-buffer))
            (switch-to-buffer-other-window nyxt-buffer))
        (switch-to-buffer nyxt-buffer)))))

(cl-defun nyxt-run (sexps &key (focus nil) (autostart nil) (autostart-delay 0))
  "Evaluate SEXPS in the context of the current Nyxt connection.

If FOCUS, change focus to the Nyxt exwm workspace.  If AUTOSTART is non-nil
and a Nyxt system process is not found, it will automatically create one and
connect Sly to it.

Additionally, you may specify an AUTOSTART-DELAY to invoke Nyxt features that
might require some delay to be correctly loaded."
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
             (run-at-time autostart-delay nil
                          (lambda ()
                            (while (or (not (sly-connected-p))
                                       (not (nyxt--sly-connected-p))
                                       (not nyxt-sly-connection))
                              (sleep-for 0.1)
                              (nyxt-sly-connect))
                            (and focus (nyxt--exwm-focus-window))
                            (nyxt--sly-eval sexps :startup t))))
            ((or (string-match (rx (+ any) "Deleting socket") output)
                 (/= (process-exit-status process) 0))
             (setq nyxt-process nil)
             (setq nyxt-sly-connection nil)))))))
     ((or (nyxt--system-process-p)
          nyxt-process)
      (let ((sly-default-connection (or nyxt-sly-connection (nyxt-sly-connect))))
        (and focus (nyxt--exwm-focus-window))
        (nyxt--sly-eval sexps))))))

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

(defun nyxt-store-link ()
  "Store the current page link via Org mode."
  (when (and (or nyxt-process
                 (nyxt--system-process-p))
             (when (require 'exwm nil t)
               (string-match "Nyxt:" (buffer-name (current-buffer)))))
    (require 'ol)
    (org-link-store-props
     :type "nyxt"
     :link  (if (nyxt--extension-p "nx-router" "trace-url")
                (nyxt--sly-eval
                 '(render-url (nx-router:trace-url (url (current-buffer))))
                 :return t)
              (nyxt--sly-eval '(render-url (url (current-buffer))) :return t))
     :description (nyxt--sly-eval '(title (current-buffer)) :return t))))

;;;###autoload
(defun nyxt-sly-connect ()
  "Connect to a Slynk server via Sly to interact with the Nyxt browser."
  (interactive)
  (setq nyxt-sly-connection (sly-connect "localhost" nyxt-port)))

;;;###autoload
(defun nyxt-init ()
  "Start Nyxt and focus on its window."
  (interactive)
  (nyxt-run nil :focus t :autostart t))

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
(cl-defun nyxt-capture (template &key (roam-p nil))
  "Store and capture the current Nyxt page link in Org TEMPLATE.

If ROAM-P, store it in the corresponding Org Roam capture TEMPLATE."
  (interactive)
  (with-current-buffer
      (car (cl-remove-if-not (lambda (buffer)
                               (string-match "Nyxt:"
                                             (buffer-name buffer)))
                             (buffer-list)))
    (org-store-link t t)
    (if roam-p
        (org-roam-capture nil template)
      (org-capture nil template))))

;;;###autoload
(defun nyxt-search (query)
  "Search for QUERY in the default search engine in Nyxt."
  (interactive "sSearch for: ")
  (nyxt-run
   `(buffer-load
     (first (nyxt::input->queries ,query))
     :buffer (make-buffer-focus))
   :focus t :autostart t :autostart-delay 2))

;;;###autoload
(defun nyxt-load-theme (theme)
  "Load THEME in Nyxt."
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
         '(mapcar #'tailor::id
                  (tailor:themes (tailor::current-tailor-mode))))))))))
  (if (nyxt--extension-p "nx-tailor" "load-theme")
      (nyxt-run `(nx-tailor:load-theme ',theme))
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
   '(nyxt/document-mode::scroll-down)))

;;;###autoload
(defun nyxt-scroll-other-window-down ()
  "Scroll the Nyxt window upward."
  (interactive)
  (nyxt-run
   '(nyxt/document-mode::scroll-up)))

;;;###autoload
(defun nyxt-set-transient-map ()
  "Set a transient map for transient `nyxt' commands."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "v" 'nyxt-scroll-other-window)
     (define-key map "V" 'nyxt-scroll-other-window-down)
     map)
   t))

;;;###autoload
(defun nyxt-default-keybindings ()
  "Bind the `C-c y' prefix to `nyxt' commands."
  (interactive)
  (define-key mode-specific-map "y" 'nyxt-map))

(define-prefix-command 'nyxt-map)
(let ((map nyxt-map))
  (define-key map "y" #'nyxt-sly-connect)
  (define-key map "i" #'nyxt-init)
  (define-key map "q" #'nyxt-quit)
  (define-key map "s" #'nyxt-search)
  (define-key map "w" #'nyxt-copy-url)
  (define-key map "k" #'nyxt-delete-current-buffer)
  (define-key map "v" #'nyxt-set-transient-map))

(provide 'nyxt)
;;; nyxt.el ends here
