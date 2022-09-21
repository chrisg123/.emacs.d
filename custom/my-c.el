;;; my-c.el --- C customization
;;; Commentary:

;;; Code:
(defvar c-mode-base-map)
(require 'tree-sitter)
(require 'tree-sitter-hl)

(add-hook
 'c-mode-common-hook
 (lambda()
   (semantic-mode 1)
   (tree-sitter-hl-mode)
   (setq-default flycheck-checker 'c/c++-gcc)
   (define-key c-mode-base-map
     (kbd "M-.") 'semantic-ia-fast-jump)))

(provide 'my-c)
;;; my-c.el ends here
