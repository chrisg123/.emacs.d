;;; my-lsp.el --- Lsp customization
;;; Commentary:

;;; Code:
(require 'lsp-mode)
(require 'lsp-ui)

(lsp-ui-flycheck-add-mode 'kotlin-mode)
(add-to-list 'flycheck-checkers 'lsp-ui)

(defun lsp-mode-keybinding()
  "Setup keybinding for lsp-mode."
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (local-set-key (kbd "M-?") 'lsp-find-references)
  (local-set-key (kbd "C-c C-f") 'lsp-format-region))

(defun lsp-mode-settings()
  "Custom lsp-mode settings."
  (setq lsp-log-io t))

(add-hook 'lsp-mode-hook
	  (lambda()
	    (lsp-mode-keybinding)
	    (lsp-mode-settings)
	    (lsp-ui-mode)
	    ))

(add-hook 'kotlin-mode-hook
	  (lambda()
	    (lsp)
	    (flycheck-mode)
	    ))

(provide 'my-lsp)

;;; my-lsp.el ends here
