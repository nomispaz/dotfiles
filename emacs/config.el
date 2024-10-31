(add-to-list 'load-path "~/.config/emacs/site-lisp")

(setq backup-directory-alist '((".*" . "~/.local/share/emacs/backups")))

(when (getenv "WAYLAND_DISPLAY")
(setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe
                                        :noquery t))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
(defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
        (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste))

(electric-pair-mode 1)

(setq inhibit-startup-screen t)

(tool-bar-mode -1)	          ;; Disable the toolbar

(menu-bar-mode -1)            ;; Disable the menu bar

;;  (scroll-bar-mode -1)          ;; Disable visible scrollbar

(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 180)

(set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono" :height 180)

(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 180)

(global-set-key (kbd "C-x C-g") 'rgrep)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(require 'spacemacs-theme)
(load-theme 'spacemacs-dark :no-confirm)

(recentf-mode 1)

(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1)

(require 'desktop)
(desktop-save-mode 1)
;; don't save the following buffers
   (add-to-list 'desktop-modes-not-to-save 'dired-mode)
   (add-to-list 'desktop-modes-not-to-save 'Info-mode)
   (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
   ;; specify dir to save session
   desktop-dirname "~/.local/share/emacs/emacs_session_backup"
   desktop-base-file-name "desktop"
   desktop-base-lock-name "desktop.lock"

(require 'which-key)
(which-key-mode 1)
   (setq which-key-idle-delay 0.3)
  (setq which-key-allow-evil-operators t)

(defun my/org-mode-setup()
  ;; active automatic indentation
  (org-indent-mode)
  ;; proportially resize font
  (variable-pitch-mode 1)
  ;; automatically perform line wrap
  (visual-line-mode 1)
  )

(require 'org)
(add-hook 'org-mode 'my/org-mode-setup)
   ;; replace "..." at the end of collapsed headlines
    (setq org-ellipsis " ▾"
          ;; remove special characters used for bold, kursiv etc.
          org-hide-emphasis-markers t)

    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    ;; RETURN will follow links in org-mode files
    (setq org-return-follows-link  t)  
    ;; (setq org-agenda-files
    ;; 	'("/mnt/nvme2/orgmode/")
    ;; 	)
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;;Set faces for heading levels.
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.05)
                    (org-level-4 . 1.0)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    ;;:bind (;;copy link anker to clipboard, insert with C-c C-l
      ;;     ("C-c l" . org-stored-links))

(require 'org-agenda)
    (setq org-agenda-files (directory-files-recursively "/mnt/d/WSL/orgmode" "\\.org$"))
