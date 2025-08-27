(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.config/emacs/site-lisp")

;; first remap major modes to major-ts-mode
 (setq major-mode-remap-alist
   '((elixir-mode . elixir-ts-mode)
    (go-mode . go-ts-mode)
(rust-mode . rust-ts-mode)
(java-mode . java-ts-mode)))

 (setq inhibit-startup-screen t)   ;; Disable the welcome screen
 (tool-bar-mode -1)   	            ;; Disable the toolbar
 (menu-bar-mode -1)                ;; Disable the menu bar
 (scroll-bar-mode -1)              ;; Disable visible scrollbar
 (pixel-scroll-precision-mode 1) ;; enable smooth scrolling

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

 ;; disable sound
 (setq ring-bell-function 'ignore)

 ;; set backup and autosave folders
 (make-directory "~/.local/share/emacs/autosave/" t)
 (make-directory "~/.local/share/emacs/backups/" t)
 (setq auto-save-file-name-transforms '((".*" "~/.local/share/emacs/autosave/" t)))
 (setq backup-directory-alist `(("." . "~/.local/share/emacs/backups/")))

 ;; copy the current file instead of moving and then copying back
 (setq backup-by-copying t)

 ;; remove need to set two spaces at the end of sentences
 (setq sentence-end-double-space nil)

 ;; disable automatic resizing of the frame
 (setq frame-inhibit-implied-resize t)

 ;; Highlight trailing whitespace.
 (setq-default show-trailing-whitespace t)
 (set-face-background 'trailing-whitespace "yellow")

 ;; enter y or n instead of yes/no
 (defalias 'yes-or-no-p 'y-or-n-p)

 (setq indent-tabs-mode nil) ;; no tab

 (setq create-lockfiles nil) ;; no need to create lockfiles

;; (require 'nomispaz)

(global-set-key (kbd "C-+") 'text-scale-increase)                ;; zoom in
   (global-set-key (kbd "C--") 'text-scale-decrease)                ;; zoom out
   (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)       ;; zoom in with mouse wheel
   (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)     ;; zoom out with mouse wheel
 ;;copy link anker to clipboard, insert with C-c C-l
 (global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; escape quits everything
 ;; duplicate current line
;; first unbind the C-, map in orgmode, then redefine the keymap
 (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-,") nil))

 (global-set-key (kbd "C-,") 'duplicate-line)
 (global-set-key (kbd "C-x TAB") 'indent-region)

(defun move-line-up ()
    (interactive)
    (transpose-lines 1)
    (forward-line -2))

  (defun move-line-down ()
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1))

  (global-set-key (kbd "M-<up>") 'move-line-up)
  (global-set-key (kbd "M-<down>") 'move-line-down)

  (defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up n)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down n)))

(global-set-key (kbd "M-p") 'move-line-region-up)
(global-set-key (kbd "M-n") 'move-line-region-down)

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

(set-face-attribute 'mouse nil :background "white")

(require 'catppuccin-theme)
(load-theme 'catppuccin :no-confirm)

;; Define a helper function to display a popup menu with all commands for a mode
(defun my/display-mode-menu (mode)
  "Display a popup menu with all commands available for MODE."
  (let ((mode-map (symbol-function mode)))
    (if (keymapp mode-map)
        (popup-menu
         (easy-menu-create-menu
          (symbol-name mode)
          (cl-loop for key in (cdr mode-map)
                   for binding = (cdr key)
                   when (commandp binding)
                   collect (vector (symbol-name binding) binding))))
      (message "No command menu available for %s" (symbol-name mode)))))

;; Helper function to make clickable modeline text with a popup menu
(defun my/modeline-menu-clickable (text mode)
  "Return TEXT with MODE set as a clickable action to show the mode's commands in the mode line."
  (propertize text 'mouse-face 'mode-line-highlight
              'help-echo (concat "Click to see commands for " (symbol-name mode))
              'local-map (let ((map (make-sparse-keymap)))
                           ;; Use a dynamically created function to avoid lexical binding
                           (define-key map [mode-line mouse-1]
                             `(lambda () (interactive) (my/display-mode-menu ',mode)))
                           map)))

;; Define a custom modeline
(defun my/custom-evil-mode-line-indicator ()
  "Return a string for the current Evil mode state."
  (cond
   ((evil-normal-state-p) "N")
   ((evil-visual-state-p) "V")
   ((evil-insert-state-p) "I")
   (t "-")))

(setq-default mode-line-format
              '((:eval (concat
                        " "
                        ;; Evil mode indicator
                        (my/custom-evil-mode-line-indicator)
                        " "

                        ;; Buffer name
                        "%b "
                        
                        ;; Line number
                        "L%l "
                        
                        ;; Yasnippet
                        (when (bound-and-true-p yas-minor-mode)
                          (my/modeline-menu-clickable " Yas " 'yas-minor-mode))
                        
                        ;; Flymake
                        (when (bound-and-true-p flymake-mode)
                          (my/modeline-menu-clickable " Flymake " 'flymake-mode))

                        ;; Go mode
                        (when (derived-mode-p 'go-mode)
                          (my/modeline-menu-clickable " Go " 'go-mode))

                        ;; Rust mode
                        (when (derived-mode-p 'rust-mode)
                          (my/modeline-menu-clickable " Rust " 'rust-mode))

                        ;; Python mode
                        (when (derived-mode-p 'python-mode)
                          (my/modeline-menu-clickable " Python " 'python-mode))))))

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
    completion-styles '(basic flex)
    icomplete-in-buffer t
    max-mini-window-height 10)

(fido-vertical-mode 1)

(require 'yasnippet)
(require 'yasnippet-snippets)
(yas-global-mode 1)
(global-set-key (kbd "C-c C-s") 'yas-insert-snippet)

(require 'cape)
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  (global-set-key (kbd "C-c p") 'cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  ;; (add-hook 'completion-at-point-functions #'cape-history)

(setq completion-at-point-functions
      (list (cape-capf-super
                       #'cape-keyword
		       #'cape-file
                       #'cape-dabbrev
		      (cape-company-to-capf #'company-yasnippet))
            completion-at-point-functions))

;;;; Merge Cape with Eglot's completions
;;(defun my/setup-cape-with-eglot ()
;;  "Use Cape sources in addition to Eglot completions."
;;  (setq-local completion-at-point-functions
;;              (list (cape-capf-super
;;                     #'eglot-completion-at-point
;;                     #'cape-keyword
;;                     #'cape-file
;;                     #'cape-dabbrev
;;                     (cape-company-to-capf #'company-yasnippet)))))
;;
;;(add-hook 'eglot-managed-mode-hook #'my/setup-cape-with-eglot)

;; Ensure it uses minibuffer completion
(setq completion-in-region-function
      (lambda (&rest args)
        (apply #'consult-completion-in-region args))) ;; if you have consult
;; Or if you don't have consult:
;; (setq completion-in-region-function #'completion--in-region)

(require 'markdown-mode)

(require 'consult)
(setq recentf-mode 1)

(require 'eglot)
  (require 'breadcrumb)
(defun add-yasnippet
    ()
    (setq company-backends '((company-capf :with company-yasnippet))))
(add-hook 'eglot--managed-mode-hook #'add-yasnippet)
(add-to-list 'eglot-server-programs '(elixir-mode "/usr/bin/language_server.sh"))
(add-to-list 'eglot-server-programs '(java-mode . (lambda (i p) (list "~/.local/share/jdtls/bin/jdtls" "-configuration" "~/.local/share/jdtls/config_linux"))))

(defun my/setup-local-jdtls ()
  "Ensure ~/.local/share/jdtls exists and is up to date with /usr/libexec/jdtls.
Also copy the config_linux folder from /usr/share/jdtls only if it is newer."
  (let* ((local-dir (expand-file-name "~/.local/share/jdtls"))
         (system-dir "/usr/libexec/jdtls")
         (config-src "/usr/share/jdtls/config_linux")
         (config-dest (expand-file-name "config_linux" local-dir))
         (local-exists (file-directory-p local-dir)))
    ;; Step 1: ensure JDTLS exists locally
    (cond
     ((not local-exists)
      (message "JDTLS: copying fresh install from %s → %s ..." system-dir local-dir)
      (copy-directory system-dir local-dir t t t)
      (message "JDTLS: installed locally at %s" local-dir))
     (t
      (let* ((local-time (nth 5 (file-attributes local-dir)))
             (system-time (nth 5 (file-attributes system-dir))))
        (when (time-less-p local-time system-time)
          (message "JDTLS: system version is newer, refreshing local copy...")
          (delete-directory local-dir t)
          (copy-directory system-dir local-dir t t t)
          (message "JDTLS: refreshed local copy at %s" local-dir)))))

    ;; Step 2: copy config_linux only if system version is newer or missing locally
    (when (file-directory-p config-src)
      (let ((copy-needed
             (or (not (file-directory-p config-dest))
                 (time-less-p (nth 5 (file-attributes config-dest))
                              (nth 5 (file-attributes config-src))))))
        (when copy-needed
          (message "JDTLS: copying config_linux from %s → %s ..." config-src config-dest)
          (when (file-directory-p config-dest)
            (delete-directory config-dest t))
          (copy-directory config-src config-dest t t t)
          (message "JDTLS: config_linux copied to %s" config-dest))))))

;; Run at startup
(my/setup-local-jdtls)

; tree-sitter setup languages
    (setq treesit-language-source-alist
          '((go "https://github.com/tree-sitter/tree-sitter-go")
	    (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
	    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	    (heex "https://github.com/phoenixframework/tree-sitter-heex")
	    (java "https://github.com/tree-sitter/tree-sitter-java"))
	  )
(defun my/install-treesit_languages()
 (interactive)
 (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 )

; Enable lsp-mode for Go and Rust modes
(require 'go-mode)
  (setq indent-tabs-mode nil)
  (setq go-announce-deprecations t)
  (setq go-mode-treesitter-derive t)

(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'breadcrumb-local-mode)

(require 'rust-mode)
(setq indent-tabs-mode nil)
 (setq rust-mode-treesitter-derive t)

(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook
  (lambda () (setq indent-tabs-mode nil)))  
(add-hook 'rust-mode-hook 'yas-minor-mode)
(add-hook 'rust-mode-hook 'breadcrumb-local-mode)
(setq rust-format-on-save t)

; Enable lsp-mode for Go and Rust modes
(setq java-mode-treesitter-derive t)
(add-hook 'java-ts-mode-hook 'eglot-ensure)
(add-hook 'java-ts-mode-hook 'yas-minor-mode)
(add-hook 'java-ts-mode-hook 'breadcrumb-local-mode)

;; this is necessary since elixir-ts-mode doesn't start automatically when an elixir-file is opened in contrast to elixir-mode
 (add-to-list 'auto-mode-alist '("\\.ex\\'"  . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))

       (setq indent-tabs-mode nil)
     (setq elixir-announce-deprecations t)
        (setq elixir-mode-treesitter-derive t)
        (add-hook 'elixir-ts-mode-hook'
                (lambda () (setq indent-tabs-mode nil)))
      (add-hook 'elixir-ts-mode-hook 'eglot-ensure)
      (add-hook 'elixir-ts-mode-hook 'yas-minor-mode)
      (add-hook 'elixir-ts-mode-hook 'breadcrumb-local-mode)

(setq project-vc-extra-root-markers '(".project.el"))
(require 'project)

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
;,(setq org-agenda-files (directory-files-recursively "/mnt/nvme2/data/orgmode" "\\.org$"))

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

(require 'evil)
   (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (evil-mode 1)

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
   (evil-set-leader 'normal (kbd "SPC"))
   (evil-set-leader nil (kbd "SPC"))

   ;; set local leader
   (evil-set-leader 'normal "," t)

  ;; window navigation
    (define-key evil-normal-state-map (kbd "C-w <right>") '("Change to right window" . evil-window-right))
    (define-key evil-normal-state-map (kbd "C-w <left>") '("Change to left window" . evil-window-left))
   (define-key evil-normal-state-map (kbd "C-w <up>") '("Change to upper window" . evil-window-top))
   (define-key evil-normal-state-map (kbd "C-w <down>") '("Change to bottom window" . evil-window-down))
    (define-key evil-normal-state-map (kbd "C-w k") '("Close window" . evil-window-delete)) 
  ;; files
   (define-key evil-normal-state-map (kbd "<leader> f f") '("Search files" . consult-find))
   (define-key evil-normal-state-map (kbd "<leader> f r") '("Recent files" . recentf))
   (define-key evil-normal-state-map (kbd "<leader> f g") '("Search files (grep)" . consult-grep))
   (define-key evil-normal-state-map (kbd "<leader> f n") '("New file" . evil-buffer-new))

   ;; buffers
   (define-key evil-normal-state-map (kbd "<leader> b b") '("Switch to buffer" . switch-to-buffer))
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
(define-key evil-normal-state-map (kbd "K") '("LSP show doc in buffer" . eldoc))
(define-key evil-normal-state-map (kbd "C-.") '("LSP execute code action" . eglot-code-actions))
