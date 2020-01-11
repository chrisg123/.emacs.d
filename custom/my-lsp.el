;;; my-lsp.el --- Lsp customization
;;; Commentary:

;;; Code:
(require 'lsp-mode)

(add-hook 'kotlin-mode-hook #'lsp)

(provide 'my-lsp)

;;; my-lsp.el ends here
