(use-package emacs
    :init
    ;; always show line and column numbers
    ;; display line numbers but not in specific view modes
   
;; activate autosuggestions
;; org-mode


;; vim key-bindings

;; -----------------------------------------------------------------------

;; hooks and settings outside of packages

;; activate company in all modes
;; reload yas-snippets

;;python-mode-hooks

;;elisp-mode-hooks

;; hooks for org-mode files
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
