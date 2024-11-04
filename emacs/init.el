(add-to-list 'load-path "~/.config/emacs/site-lisp/")

(setq inhibit-startup-screen t)   ;; Disable the welcome screen
  (tool-bar-mode -1)   	            ;; Disable the toolbar
  (menu-bar-mode -1)                ;; Disable the menu bar
;;  (scroll-bar-mode -1)              ;; Disable visible scrollbar

  ;; Remember recently edited files. Can then be shown with recentf-open-files
  (recentf-mode 1)

  ;; Save what you enter into minibuffer prompts
  (setq history-length 25)
  (savehist-mode 1)

  ;; Remember and restore the last cursor location of opened files
  (save-place-mode 1)

  ;; show relative line numbers
  (menu-bar--display-line-numbers-mode-relative)

  ;; automatically close brackets
  (electric-pair-mode 1)

;; (require 'nomispaz)

(global-set-key (kbd "C-+") 'text-scale-increase)                ;; zoom in
  (global-set-key (kbd "C--") 'text-scale-decrease)                ;; zoom out
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)       ;; zoom in with mouse wheel
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)     ;; zoom out with mouse wheel
;;copy link anker to clipboard, insert with C-c C-l
(global-set-key (kbd "C-c l") 'org-store-link)

(require 'desktop)
  (desktop-save-mode 1)
   ;; don't save the following buffers
   (add-to-list 'desktop-modes-not-to-save 'dired-mode)
   (add-to-list 'desktop-modes-not-to-save 'Info-mode)
   (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
   ;; specify dir to save session
   (setq desktop-dirname "~/.local/share/emacs/emacs_session_backup")
   (setq desktop-base-file-name "desktop")
   (setq desktop-base-lock-name "desktop.lock")

(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 180)
(set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono" :height 180)
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 180)

(load-theme 'wombat)

;; display completions in one column in minibuffer
(setq completions-format 'one-column)
;; disable header for completions (shown number of possible completions)
(setq completions-header-format nil)
;; disables case-sensitivity for minibuffer searches
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

(setq completion-auto-wrap t
    completion-auto-help nil
    completions-max-height 15
    completion-styles '(flex)
    icomplete-in-buffer t
    max-mini-window-height 10)

(fido-vertical-mode 1)

(require 'org)
(require 'org-agenda)

;; replace "..." at the end of collapsed headlines
(setq org-ellipsis " ▾"
;; remove special characters used for bold, kursiv etc.
org-hide-emphasis-markers t)

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
;; RETURN will follow links in org-mode files
(setq org-return-follows-link  t)  

(add-hook 'org-mode-hook 'my/org-mode-setup())
(add-hook 'org-mode-hook 'my/org-font-setup())

;; folder for org-agenda
(setq org-agenda-files (directory-files-recursively "/mnt/d/WSL/orgmode" "\\.org$"))

(defun my/org-mode-setup()
  ;; active automatic indentation
  (org-indent-mode 1)
  ;; proportially resize font
  (variable-pitch-mode 1)
  ;; automatically perform line wrap
  (visual-line-mode 1)
)
  (defun my/org-font-setup()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;;Set faces for heading levels.
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
(set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height (cdr face)))
;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  )
