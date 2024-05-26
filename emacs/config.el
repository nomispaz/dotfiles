(setq backup-directory-alist '((".*" . "~/.local/share/emacs/backups")))

  (setq major-mode-remap-alist
              '((bash-mode . bash-ts-mode)
                (python-mode . python-ts-mode)))


  (unless (package-installed-p 'use-package)
     (package-install 'use-package))

  (require 'package)

  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

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
(setq interprogram-paste-function 'wl-paste)

  (use-package spacemacs-theme
      :ensure t
      :config
      (load-theme 'spacemacs-dark t)
      )

(use-package nerd-icons
  :ensure t)

  (setq inhibit-startup-screen t)

  (tool-bar-mode -1)	          ;; Disable the toolbar

  (menu-bar-mode -1)            ;; Disable the menu bar

  (scroll-bar-mode -1)          ;; Disable visible scrollbar

    (column-number-mode 1)

     (unless (derived-mode-p 'image-mode 'doc-view-mode 'pdf-view-mode)
      (global-display-line-numbers-mode 1)
      )

    (dolist (mode '(org-mode-hook
		    term-mode-hook
		    shell-mode-hook
		    eshell-mode-hook))
      (add-hook mode (lambda() (display-line-numbers-mode 0))))

  (electric-pair-mode 1)

    (delete-selection-mode 1)    ;; You can select text and delete it by typing.
    (electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
    ;; The following prevents <> from auto-pairing when electric-pair-mode is on.
    ;; Otherwise, org-tempo is broken when you try to <s TAB...
    (add-hook 'org-mode-hook (lambda ()
               (setq-local electric-pair-inhibit-predicate
                       `(lambda (c)
                      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.
  (setq use-dialog-box nil)    ;; No dialog box
(setq pop-up-windows nil)    ;; No popup windows

  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 180)

  (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono" :height 180)

  (set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 180)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

  (use-package company
    :ensure t
    :custom
    (company-idle-delay .1)
    (company-minimum-prefix-length 2)
    (company-show-numbers t)
    (company-tooltip-align-annotations 't)
    (global-company-mode t)
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

(use-package consult
  :ensure t
  :config
  (recentf-mode 1)
)

(defvar-local my-modeline-file-status
    '(:eval
      ;; insert save icon from nerd-fonts when buffer was changed
      (if (buffer-modified-p) 
	 (propertize (format " %s" (nerd-icons-mdicon "nf-md-content_save_edit")))
      )
      )
  )

(defvar-local my-modeline-buffer-name
    '(:eval
     (propertize (format " %s " (buffer-name)) 'face 'mode-line-buffer-id)
     )
  )

(defvar-local my-modeline-mode-name
    '(:eval
        (propertize 
	 (format-mode-line mode-name)
	 'help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
	 'mouse-face 'spacemacs-theme-custom-colors
	 'local-map mode-line-major-mode-keymap)
	)
  )

(defvar-local my-modeline-evil-state
    '(:eval (cond
       (( eq evil-state 'visual) "V")
       (( eq evil-state 'normal) "N")
       (( eq evil-state 'insert) "I")
       (t "*")))
    )

(defvar-local my-modeline-flycheck
   '(:eval
     (when (and (bound-and-true-p flycheck-mode)
              (or flycheck-current-errors
                  (eq 'running flycheck-last-status-change)))
	 ;;(bound-and-true-p t)
	   (propertize (format " FlyC " )
	    'help-echo "Flycheck "
	    'mouse-face 'spacemacs-theme-comment-bg
                       'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map [mode-line down-mouse-1]
                                      flycheck-mode-menu-map)
                                    (define-key map [mode-line mouse-2]
                                      (lambda ()
                                        (interactive)
                                        (describe-function 'flycheck-mode)))
                                    map))
       ))
   )

(defun my/flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))

(defvar-local my-modeline-flycheck-errors
'(:eval
   (when (and (bound-and-true-p flycheck-mode)
              (or flycheck-current-errors
                  (eq 'running flycheck-last-status-change)))
     (concat
      (cl-loop for state in '((error . "#e0211d")
                              (warning . "#dc752f")
                              (info . "#83A598"))

               as lighter = (my/flycheck-lighter (car state))
               when lighter
               concat (propertize
                       lighter
                       'face `(:foreground ,(cdr state))))
      " "))))

(dolist (construct '(my-modeline-buffer-name
		     my-modeline-file-status
		     my-modeline-mode-name
		     my-modeline-flycheck
		     my-modeline-flycheck-errors
		     my-modeline-evil-state
                     ))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
	      '(;; error-message
		"%e"			
		mode-line-front-space
		my-modeline-evil-state
		" "
		;; display save icon if buffer was changed
		my-modeline-file-status
		;; display buffer name
		my-modeline-buffer-name
		;; display row and column numbers
		mode-line-position-column-line-format
		" "
	        my-modeline-mode-name
		my-modeline-flycheck
		my-modeline-flycheck-errors
		;; show git status
		vc-mode
		" "
		mode-line-end-spaces
		)
	      )

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

  (use-package which-key
    :ensure t
    :init
    (which-key-mode 1)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.3)
    (setq which-key-allow-evil-operators t)
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
  ;; 	'("/data/orgmode/")
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
    (setq org-agenda-files (directory-files-recursively "/data/orgmode/" "\\.org$"))
    )

  (use-package evil
    :ensure t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1)
    )

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
   (define-key evil-normal-state-map (kbd "<leader> l l") '("Show list of flycheck errors" . flycheck-list-errors))
   (define-key evil-normal-state-map (kbd "<leader> l n") '("Next flycheck error" . flycheck-next-error))
   (define-key evil-normal-state-map (kbd "<leader> l p") '("Previous flycheck error" . flycheck-previous-error))

(setq treesit-language-source-alist
	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	    (python "https://github.com/tree-sitter/tree-sitter-python"))
	  )


(defun my/install-treesit_languages()
 (interactive)
 (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 )

(use-package python-ts-mode
  :mode "\\.py\\'"
  )

(use-package yasnippet
  :ensure t
  )

;; load prepared snippets
(use-package yasnippet-snippets
  :ensure t
  )

(use-package eglot
   :ensure t
   :hook
   (python-ts-mode . eglot-ensure)
  )

  (use-package flycheck
    :ensure t
    :config
    (setq-default flycheck-flake8-maximum-line-length 200))

(yas-reload-all)

(add-hook 'python-mode-hook 'yas-minor-mode)

(add-hook 'elisp-mode-hook 'yas-minor-mode)

(add-hook 'org-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'org-superstar-mode)