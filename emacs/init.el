(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path "~/.config/emacs/site-lisp/")

(setq inhibit-startup-screen t)   ;; Disable the welcome screen
(tool-bar-mode -1)   	            ;; Disable the toolbar
(menu-bar-mode -1)                ;; Disable the menu bar
(scroll-bar-mode -1)              ;; Disable visible scrollbar

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

(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-allow-evil-operators t)
  )

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

(use-package catppuccin-theme
    :ensure t
    :config
    (load-theme 'catppuccin :no-confirm))

(use-package doom-modeline
      :ensure t
      :init (doom-modeline-mode 1))

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

(use-package marginalia
  :ensure t
  :config
    (marginalia-mode 1)
)

(use-package yasnippet
  :ensure t
)
(use-package yasnippet-snippets
  :ensure t)
(yas-global-mode 1)

; Enable company-mode with language server support
        (use-package company
          :ensure t
          :custom
          (company-minimum-prefix-length 2)
        )
        (add-hook 'after-init-hook 'global-company-mode)
    (add-to-list 'company-backends '(company-capf company-yasnippet company-files))
 (add-hook 'eglot-managed-mode-hook (lambda ()

(add-to-list 'company-backends
'(company-capf :with company-yasnippet))))

(use-package consult
  :ensure t
  :config
  (recentf-mode 1)
)

(require 'eglot)
(require 'breadcrumb)

; Enable lsp-mode for Go and Rust modes
(use-package go-mode
  :ensure t
  :after lsp-mode
  :init
  (setq indent-tabs-mode nil)
  (setq go-announce-deprecations t)
  (setq go-mode-treesitter-derive t)
  )

(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'breadcrumb-local-mode)

(use-package rust-mode
  :ensure t
 :after lsp-mode
 :init
  (setq indent-tabs-mode nil)
 ;;  (setq rust-mode-treesitter-derive t)
)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook
  (lambda () (setq indent-tabs-mode nil)))  
(add-hook 'rust-mode-hook 'yas-minor-mode)
(add-hook 'rust-mode-hook 'breadcrumb-local-mode)
(setq rust-format-on-save t)

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
(setq org-agenda-files (directory-files-recursively "/mnt/nvme2/data/orgmode" "\\.org$"))

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

(use-package evil
    :ensure t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1)
    )

(evil-set-undo-system 'undo-redo)

  ;;(use-package evil-collection
  ;;  :after evil
  ;;  :ensure t
  ;;  :config
  ;;  (evil-collection-init))

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
   (define-key evil-normal-state-map (kbd "<leader> l n") '("Next flycheck error" . flymake-goto-next-error))
   (define-key evil-normal-state-map (kbd "<leader> l p") '("Previous flycheck error" . flymake-goto-previous-error))

  ;; lsp
   (define-key evil-normal-state-map (kbd "<leader> g r n") '("Rename variable or function" . eglot-rename))
(define-key evil-normal-state-map (kbd "<leader> g d") '("LSP goto definition" . xref-find-definitions))
(define-key evil-normal-state-map (kbd "<leader> g D") '("LSP Find references" . xref-find-references))
(define-key evil-normal-state-map (kbd "<leader> g s") '("LSP show doc in buffer" . eldoc))
(define-key evil-normal-state-map (kbd "C-.") '("LSP execute code action" . eglot-code-actions))
