;;; nyxt.el --- Emacs integration with Nyxt  -*- lexical-binding: t; -*-

;;; Copyright ©2022 conses <contact@conses.eu>

;;; Commentary:
;; Helper functions to interact with a Nyxt process.

;;; Code:

(require 'sly)
(require 'ol)
(require 'cl-lib)

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
  "Hold the current Nyxt process.")

(defvar nyxt-map nil
  "Map to bind `nyxt' commands to.")

;;;###autoload
(defun nyxt-connect-to-slynk ()
  "Connect to the Slynk server to interact with the Nyxt browser."
  (interactive)
  (sly-connect "localhost" nyxt-port))

;;;###autoload
(defun nyxt--slynk-connected-p ()
  "Indicate whether there's currently a connection to `nyxt-port'."
  (cl-find-if (lambda (p)
                (= (sly-connection-port p) nyxt-port))
              sly-net-processes))

;;;###autoload
(cl-defun nyxt-sly-eval (sexps &rest args &key &allow-other-keys)
  "Evaluate SEXPS and ARGS with Slynk.
It automatically attaches a Slynk process if needed."
  (let ((sly-default-connection (or (nyxt--slynk-connected-p)
                                    (nyxt-connect-to-slynk)))
        (sexp (if (every #'consp sexps)
                  (mapconcat #'prin1-to-string sexps "")
                (prin1-to-string sexps))))
    (apply #'sly-eval `(slynk:interactive-eval-region ,sexp) args)))

(defun nyxt--system-process-p ()
  "Return non-nil if the Nyxt system process is currently running."
  (cl-some (lambda (pid)
             (string-match (rx (: (* any) "nyxt" (* any)))
                           (assoc-default 'comm (process-attributes pid))))
           (list-system-processes)))

;;;###autoload
(cl-defun nyxt-exwm-focus-window (&key (focus nil))
  "Handle Nyxt's EXWM window.

It focuses on Nyxt's Emacs buffer if FOCUS, and if exwm is enabled,
it switches to its corresponding workspace."
  (interactive
   (when current-prefix-arg
     (list :focus t)))
  (when (require 'exwm nil t)
    (let* ((nyxt-buffer (car (cl-remove-if-not (lambda (buffer)
                                                 (string-match "Nyxt:" (buffer-name buffer)))
                                               (buffer-list))))
           (exwm-workspace (window-frame (or (get-buffer-window nyxt-buffer t)
                                             (when focus
                                               (with-current-buffer nyxt-buffer
                                                 (set-window-buffer (frame-selected-window exwm--frame)
                                                                    (current-buffer))
                                                 (get-buffer-window (current-buffer) t)))))))
      (if focus
          (progn
            (exwm-workspace-switch (exwm-workspace--position exwm-workspace))
            (if (and (= (exwm-workspace--position exwm-workspace--current)
                        (exwm-workspace--position exwm-workspace))
                     (> (length (window-list exwm-workspace)) 1))
                (if (get-buffer-window nyxt-buffer)
                    (select-window (get-buffer-window nyxt-buffer))
                  (switch-to-buffer-other-window nyxt-buffer))
              (switch-to-buffer nyxt-buffer)))
        (when (equal (current-buffer) nyxt-buffer)
          (exwm-input-set-local-simulation-keys nil))))))

(cl-defun nyxt-run (sexps &key (focus nil) (autostart nil) (autostart-delay 0))
  "Evaluate SEXPS in the context of the current Nyxt connection.

If FOCUS, change focus to the Nyxt exwm workspace.  If AUTOSTART is non-nil
and a Nyxt system process is not found, it will automatically create one and
connect Slynk to it."
  (let* ((sly-log-events nil)
         (sly-default-connection (or (nyxt--slynk-connected-p)
                                     (when (or nyxt-process
                                               (nyxt--system-process-p))
                                       (nyxt-connect-to-slynk)))))
    (cond
     ((and (not (nyxt--system-process-p))
           (not nyxt-process)
           (not (nyxt--slynk-connected-p))
           autostart)
      (message "Launching Nyxt...")
      (setq nyxt-process (apply #'start-process "nyxt" nil nyxt-path nyxt-startup-flags))
      (set-process-filter
       nyxt-process
       (lambda (process output)
         (with-temp-buffer
           (goto-char (point-max))
           (forward-line 0)
           (cond
            ((string-match (rx (: (+ any) "Slynk server started at port")) output)
             (run-at-time autostart-delay nil
                          (lambda ()
                            (let ((sly-default-connection (nyxt-connect-to-slynk)))
                              (nyxt-exwm-focus-window :focus focus)
                              (nyxt-sly-eval sexps)))))
            ((or (string-match (rx (: (+ any) "Deleting socket")) output)
                 (/= (process-exit-status process) 0))
             (setq nyxt-process nil)))))))
     ((or (nyxt--system-process-p)
          nyxt-process)
      (with-current-buffer (sly-mrepl--find-buffer)
        (unless (string= (sly-current-package) "nyxt-user")
          (sly-mrepl--eval-for-repl '(slynk-mrepl:guess-and-set-package "nyxt-user")))
        (nyxt-exwm-focus-window :focus focus)
        (nyxt-sly-eval sexps))))))

(defun nyxt-extension-p (system &optional symbol)
  "Check if Nyxt extension SYSTEM exists in the ASDF source registry.
Optionally test if SYMBOL is bound."
  (if symbol
      (string-match "NIL" (nyxt-sly-eval `(find-symbol ,(capitalize symbol)
                                                       ,(sly-keywordify (intern system)))))
    (not (string= (downcase (nyxt-sly-eval `(asdf:find-system ,system nil))) "nil"))))

(when (require 'exwm nil t)
  (org-link-set-parameters
   "nyxt"
   :store #'nyxt-store-link))

(defun nyxt-store-link ()
  "Store the current page link via Org mode."
  (when (and (or nyxt-process
                 (nyxt--system-process-p))
             (when (require 'exwm nil t)
               (string-match "Nyxt:" (buffer-name (current-buffer)))))
    (org-link-store-props
     :type "nyxt"
     :link  (substring
             (if (nyxt-extension-p "nx-router" "trace-url")
                 (nyxt-sly-eval '(render-url (nx-router:trace-url (url (current-buffer)))))
               (nyxt-sly-eval '(render-url (url (current-buffer)))))
             1 -1)
     :description (substring
                   (nyxt-sly-eval '(title (current-buffer)))
                   1 -1))))

;;;###autoload
(defun nyxt-init ()
  "Start Nyxt and focus on its window."
  (interactive)
  (nyxt-run '(nothing) :focus t :autostart t))

;;;###autoload
(defun nyxt-quit ()
  "Quit the Nyxt process."
  (interactive)
  (nyxt-run '(quit)))

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
(defun nyxt-change-theme (theme)
  "Switch to THEME in Nyxt."
  (interactive
   (list (completing-read "Theme:" custom-known-themes)))
  (if (nyxt-extension-p "nx-tailor" "select-theme")
      (nyxt-run
       `(nx-tailor:select-theme ,theme))
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
    (define-key map "i" #'nyxt-init)
    (define-key map "q" #'nyxt-quit)
    (define-key map "s" #'nyxt-search)
    (define-key map "w" #'nyxt-copy-url)
    (define-key map "k" #'nyxt-delete-current-buffer)
    (define-key map "v" #'nyxt-set-transient-map))

(provide 'nyxt)
;;; nyxt.el ends here
