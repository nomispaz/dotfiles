;; keybindings
;; C-x r t : change mutiple lines at the same time
;; C-g : close Minibuffer
;; C-x k : kill-buffer


;; -----------------------------------------------------------------------------------------
;; general setup

;; update path so that pyright can be installed into the folder with npm install -g pyright.
(setenv "PATH" (concat (getenv "PATH") ":~/npm-packages"))
(setq exec-path (append exec-path '("~/npm-packages")))

;remap old node names to new ones with tree-sitter
(setq major-mode-remap-alist
	  '((bash-mode . bash-ts-mode)
	    (python-mode . python-ts-mode)))

; activate use-package
(require 'package)

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

;; setup them and look of emacs
(use-package emacs
    :ensure t
    :init
    ;;(tool-bar-mode -1)
    (menu-bar-mode -1)
    (when scroll-bar-mode
      (scroll-bar-mode -1))
    (load-theme 'wombat)
    (set-face-attribute 'default nil :height 160)
    (setq inhibit-startup-screen t)

    ; tree-sitter setup languages
    (setq treesit-language-source-alist
	  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	    (python "https://github.com/tree-sitter/tree-sitter-python"))
	  )

    ;autoclose brackets
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
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))
	 ;;instead overwrite standard buffer switch
         ;;("\C-xb" . consult-buffer)))

;; setup IDE
;; automatically activate python-ts-mode when opening py-files
(use-package python-ts-mode
  :mode "\\.py\\'"
  )

;; activate language snippets
(use-package yasnippet
  :ensure t)

;; buildin client for language server
(use-package eglot
  :ensure t
  :hook
  (python-ts-mode . eglot-ensure)
 )

;; activate autosuggestions
(use-package company
  :ensure t
  :config
  ;(add-to-list 'company-backends 'company-dabbrev)
  ;;  add text suggestions "company-dabbrev" to elisp suggestions
  (add-to-list 'company-backends '(company-capf :with company-dabbrev))
  )

;; -----------------------------------------------------------------------

;; hooks and settings outside of packages

;; activate company in all modes
(add-hook 'after-init-hook 'global-company-mode)
;; reload yas-snippets and activate in python-ts-mode
(yas-reload-all)
(add-hook 'python-ts-mode-hook #'yas-minor-mode)

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
 '(package-selected-packages '(company yasnippet consult orderless vertico marginalia)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
