(use-package lsp-mode
  :ensure t
  :commands lsp lsp-deferred
  :hook ((python-ts-mode . lsp-deferred))
  :config
  (lsp-enable-which-key-integration t)
  (setq-default lsp-pylsp-plugins-flake8-max-line-length 200)
  (setq-default lsp-pylsp-plugins-pycodestyle-max-line-length 200)
  )

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  )
