;; some standard keybindings to remember
;; C-x r t : change mutiple lines at the same time
;; C-g : close Minibuffer
;; C-x k : kill-buffer
;; C-x 2 : split horizontally
;; C-x 3 : split vertically
;; C-x 1 : close all other split windows
;; C-x 0 : close current split window

;; check-parents function to go to config error

;; -----------------------------------------------------------------------------------------
;; general setup

;; update path so that pyright can be installed into the folder with npm install -g pyright.
;; not necessary anymore since change from pyright to pylsp
;; emerge -ag dev-python/flake8 dev-python/python-lsp-server
;; (setenv "PATH" (concat (getenv "PATH") ":~/npm-packages/bin"))
;; (setq exec-path (append exec-path '("~/npm-packages/bin")))

;; remap old node names to new ones with tree-sitter
(setq major-mode-remap-alist
	  '((bash-mode . bash-ts-mode)
	    (python-mode . python-ts-mode)))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; activate use-package
(require 'package)

;; add package sources
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; activate integration with wl-copy
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

;; -----------------------------------------------------------------------------------------
;; packages

;; theme
(use-package spacemacs-theme
  :ensure t)

;; setup them and look of emacs
(use-package emacs
    :init
     (tool-bar-mode -1)		; Disable the toolbar
     (menu-bar-mode -1)		; Disable the menu bar
     (scroll-bar-mode -1) 	; Disable visible scrollbar
				       
    ;; load spacemacs theme. "t" to automatically confirm
    (load-theme 'spacemacs-dark t)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 180)
    (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono" :height 180)
    (set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 180)
    (setq inhibit-startup-screen t)

    ;; always show line and column numbers
    (column-number-mode 1)
    ;; display line numbers but not in specific view modes
    (unless (derived-mode-p 'image-mode 'doc-view-mode 'pdf-view-mode)
      (display-line-numbers-mode 1)
      )
    
    ;; but not in specific modes
    (dolist (mode '(org-mode-hook
		    term-mode-hook
		    shell-mode-hook
		    eshell-mode-hook))
      (add-hook mode (lambda() (display-line-numbers-mode 0))))
    
    ; tree-sitter setup languages
    (setq treesit-language-source-alist
	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	    (python "https://github.com/tree-sitter/tree-sitter-python"))
	  )

    ;; autoclose brackets
    (electric-pair-mode)
    )

;; more info for commands in minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1)
  )

;; vertical layout of the minibuffer
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1)
  )

;; pattern matching algorithm for minibuffer
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  )

;; enhanced preview and search capabilities
;; filtering of results possible. use consult-narrow-help from within the buffer
;; consult-outline to quickly jump to headers in org-mode
(use-package consult
  :ensure t
  :config
  (recentf-mode 1)
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
	 ;; Search for recent files
	 ("M-s M-r" . consult-recent-file)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))

;; -----------------------------------------------------------------------
;;config mode-line

(use-package nerd-icons
  :ensure t)

;; declare local variables for modeline

(defvar-local my-modeline-file-status
    '(:eval
      ;; insert save icon from nerd-fonts when buffer was changed
      (if (buffer-modified-p) 
	 (propertize (format " %s" (nerd-icons-mdicon "nf-md-content_save_edit")))
      )
      )
  )

(defvar-local my-modeline-buffer-name
    '(;; eval to always update the variable
      :eval
      ;; set property face (display) to the color of mode-line-buffer-id
      ;; format with spaces around name
      ;; file name with complete path
      ;; (propertize (format " %s " (buffer-file-name)) 'face 'mode-line-buffer-id)
       (propertize (format " %s " (buffer-name)) 'face 'mode-line-buffer-id)

     )
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

;; read errors and warnings from flycheck
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


;; display klickable major-mode with keybindings
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

;; state of evil-mode in mode-line
(defvar-local my-modeline-evil-state
    '(:eval (cond
       (( eq evil-state 'visual) "V")
       (( eq evil-state 'normal) "N")
       (( eq evil-state 'insert) "I")
       (t "*")))
    )

;; create list of all custom mode-line variables.
;; without setting them to risky mode, they will not work
(dolist (construct '(my-modeline-buffer-name
		     my-modeline-file-status
		     my-modeline-mode-name
		     my-modeline-flycheck
		     my-modeline-flycheck-errors
		     my-modeline-evil-state
                     ))
  (put construct 'risky-local-variable t))

;; setq-default to effect all mode-lines and not only the local one
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
	      
;; ---------------------------------------------------------------

;; show keybindings
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  )

;; save last session
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

;; setup IDE
;; automatically activate python-ts-mode when opening py-files
(use-package python-ts-mode
  :mode "\\.py\\'"
  )

;; activate language snippets
(use-package yasnippet
  :ensure t
  )

;; load prepared snippets
(use-package yasnippet-snippets
  :ensure t
  )

;; LSP
;; use either
;; -----------------------------------
;; buildin client for language server
;; (use-package eglot
;;   :ensure t
;;   :hook
;;   (python-ts-mode . eglot-ensure)
;;  )
;; ;; -----------------------------------
;; ; or
;; ;; -----------------------------------
;; lsp-mode
(use-package lsp-mode
  :ensure t
  :commands lsp lsp-deferred
  :hook ((python-ts-mode . lsp-deferred))
  :config
  (lsp-enable-which-key-integration t)
  (setq-default lsp-pylsp-plugins-flake8-max-line-length 200)
  (setq-default lsp-pylsp-plugins-pycodestyle-max-line-length 200)
  )
;; lsp ui for sideline check and peek
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  )
;; language server
;; not installed anymore since switch to pylsp
;; (use-package lsp-pyright
;;   :ensure t)
;; better integration of lsp-mode
  (use-package flycheck
    :ensure t
    :config
    (setq-default flycheck-flake8-maximum-line-length 200))
;; ---------------------------------------
;; end LSP block

;; activate autosuggestions
(use-package company
  :ensure t
  :config
					;(add-to-list 'company-backends 'company-dabbrev)
  ;;  add text suggestions "company-dabbrev" to elisp suggestions
  (add-to-list 'company-backends '(company-capf :with company-dabbrev))
  (add-to-list 'company-backends '(company-capf :with company-yasnippet))
  )

;; org-mode

;; set options for every org mode file
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
  ;; 	'("~/Documents/orgfiles/")
  ;; 	)
  (my/org-font-setup)
  :bind (;;copy link anker to clipboard, insert with C-c C-l
	 ("C-c l" . org-stored-links)
	 )
  )

(use-package org-agenda
  :config
    (setq org-agenda-files (directory-files-recursively "~/orgmode/" "\\.org$"))
    )

(use-package org-superstar
  :ensure t
  :config
  (org-superstar-mode 1)
  )

;; vim key-bindings
(use-package evil
  :ensure t
  :init
  ;; enable tab functionality for org-mode folding
  (setq evil-want-C-i-jump nil)
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

;; set leader key in all states
(evil-set-leader nil (kbd "SPC"))

;; set local leader
(evil-set-leader 'normal "," t)

;; evil keybindings
(define-key evil-normal-state-map (kbd "<leader> f f") 'consult-find)
(define-key evil-normal-state-map (kbd "<leader> f r") 'consult-recent-file)
(define-key evil-normal-state-map (kbd "<leader> f g") 'consult-grep)
(define-key evil-normal-state-map (kbd "<leader> s o") 'consult-outline)
(define-key evil-normal-state-map (kbd "<leader> s l") 'consult-line)

(define-key evil-normal-state-map (kbd "<leader> TAB b") 'consult-buffer)
;; switch to tag
(define-key evil-normal-state-map (kbd "<leader> TAB t") 'tab-switch)
;; org-mode
(define-key evil-normal-state-map (kbd "<leader> o e") 'org-export-dispatch)
(define-key evil-normal-state-map (kbd "<leader> o a") 'org-agenda)
(define-key evil-normal-state-map (kbd "<leader> o i s") 'org-schedule)
(define-key evil-normal-state-map (kbd "<leader> o t") 'org-babel-tangle)


;; -----------------------------------------------------------------------

;; hooks and settings outside of packages

;; activate company in all modes
(add-hook 'after-init-hook 'global-company-mode)
;; reload yas-snippets and activate in python-ts-mode
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)



;; ------------------------------------------------------------------------

;; functions
;; function to instal all treesitter languages that were defined above
(defun my/install-treesit_languages()
 (interactive)
 (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 )


;; -------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "eab123a5ed21463c780e17fc44f9ffc3e501655b966729a2d5a2072832abd3ac" "80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" default))
 '(org-agenda-files
   '("/home/simonheise/Documents/orgfiles/filme_und_serien.org" "/home/simonheise/Documents/orgfiles/tasks.org"))
 '(package-selected-packages
   '(org-superstar org-superstar-mode org-superstart-mode which-key company yasnippet consult orderless vertico marginalia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
