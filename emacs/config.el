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

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(electric-pair-mode 1)

(use-package spacemacs-theme
    :ensure t
    :config
    (load-theme 'spacemacs-dark t)
    )

(setq inhibit-startup-screen t)

(tool-bar-mode -1)	          ;; Disable the toolbar

(menu-bar-mode -1)            ;; Disable the menu bar

(scroll-bar-mode -1)          ;; Disable visible scrollbar

(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 180)

(set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono" :height 180)

(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 180)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package nerd-icons
  :ensure t)

(use-package desktop
  :init (desktop-save-mode 1)
  :config
   ;; don't save the following buffers
   (add-to-list 'desktop-modes-not-to-save 'dired-mode)
   (add-to-list 'desktop-modes-not-to-save 'Info-mode)
   (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
   (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
   ;; specify dir to save session
   desktop-dirname "~/emacs_session_backup"
   desktop-base-file-name "desktop"
   desktop-base-lock-name "desktop.lock"
   )

(use-package evil
    :ensure t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1)
    )

(evil-set-undo-system 'undo-redo)

  (use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))

;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t)

;; set leader key in all states
   (evil-set-leader nil (kbd "SPC"))

   ;; set local leader
   (evil-set-leader 'normal "," t)

  ;; files
   (define-key evil-normal-state-map (kbd "<leader> f f") '("Search files" . consult-find))
   (define-key evil-normal-state-map (kbd "<leader> f r") '("Recent files" . consult-recent-file))
   (define-key evil-normal-state-map (kbd "<leader> f g") '("Search files (grep)" . consult-grep))
   (define-key evil-normal-state-map (kbd "<leader> f n") '("New file" . evil-buffer-new))

   ;; buffers
   (define-key evil-normal-state-map (kbd "<leader> b b") '("Switch to buffer" . consult-buffer))
   (define-key evil-normal-state-map (kbd "<leader> b k") '("Kill current buffer" . kill-current-buffer))
   (define-key evil-normal-state-map (kbd "<leader> b r") '("Rename buffer" . rename-buffer))
   (define-key evil-normal-state-map (kbd "<leader> b s") '("Save buffer" . basic-save-buffer))

   ;; tabs
   (define-key evil-normal-state-map (kbd "<leader> t t") '("Switch to tab" . tab-switch))

   ;; search
   (define-key evil-normal-state-map (kbd "<leader> s o") '("Search heading" - consult-outline))
   (define-key evil-normal-state-map (kbd "<leader> s l") '("Search line" . consult-line))

   ;; org-mode
   (define-key evil-normal-state-map (kbd "<leader> o e") '("Export org file" . org-export-dispatch))
    (define-key evil-normal-state-map (kbd "<leader> o a") '("Open org agenda" . org-agenda))
   (define-key evil-normal-state-map (kbd "<leader> o t") '("Export code blocks" . org-babel-tangle))
   (define-key evil-normal-state-map (kbd "<leader> o i s") '("Insert scheduled date" . org-schedule))

   ;; flycheck
   (define-key evil-normal-state-map (kbd "<leader> l l") '("Show list of flycheck errors" . flymake-show-buffer-diagnostics))
   (define-key evil-normal-state-map (kbd "<leader> l n") '("Next flycheck error" . flycheck-next-error))
   (define-key evil-normal-state-map (kbd "<leader> l p") '("Previous flycheck error" . flycheck-previous-error))

  ;; lsp
   (define-key evil-normal-state-map (kbd "<leader> g r n") '("Rename variable or function" . lsp-rename))
(define-key evil-normal-state-map (kbd "<leader> g d") '("LSP goto definition" . lsp-find-declaration))
(define-key evil-normal-state-map (kbd "<leader> g D") '("LSP Find references" . lsp-find-references))
(define-key evil-normal-state-map (kbd "<leader> l d") '("LSP show doc in popup" . lsp-ui-doc-glance))

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-allow-evil-operators t)
  )

(use-package lsp-mode
	:ensure t
)

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
)

; Enable lsp-mode for Go and Rust modes
(use-package go-mode
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'go-mode-hook #'lsp)
  (setq indent-tabs-mode nil)
  (setq go-announce-deprecations t)
  (setq go-mode-treesitter-derive t))

(add-hook 'go-mode-hook 'yas-minor-mode)

(use-package rust-mode
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'rust-mode-hook #'lsp)
  (setq indent-tabs-mode nil)
  (setq rust-mode-treesitter-derive t))

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook 'yas-minor-mode)
(setq rust-format-on-save t)

(use-package yasnippet
    :ensure t
    :hook ((lsp-mode . yas-minor-mode)))
(use-package yasnippet-snippets
  :ensure t)
(yas-global-mode 1)
  (add-hook 'elisp-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'org-superstar-mode)

; Enable company-mode with language server support
    (use-package company
      :ensure t
      :custom
      (company-minimum-prefix-length 2)
    )
    (add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends '(company-capf company-yasnippet :with company-dabbrev-code))

(use-package consult
  :ensure t
  :config
  (recentf-mode 1)
)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1)
  )

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1)
  )

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  )

(defun my/org-mode-setup()
    ;; active automatic indentation
    (org-indent-mode)
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
)

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
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
  (my/org-font-setup)
  :bind (;;copy link anker to clipboard, insert with C-c C-l
	 ("C-c l" . org-stored-links)
	 )
  )

(use-package org-superstar
  :ensure t
 )

(use-package org-agenda
  :config
    (setq org-agenda-files (directory-files-recursively "/mnt/nvme2/data/orgmode" "\\.org$"))
    )
