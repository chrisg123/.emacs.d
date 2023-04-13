;;; my-latex.el --- Emacs auto save settings
;;; Commentary:
;;; Code:
(require 'tex-mode)

(add-hook 'latex-mode-hook
          (lambda()
            (setq compile-command "./build.sh")
            (setq compilation-read-command nil)
            (define-key latex-mode-map (kbd "C-c C-c") 'compile)
            ))


(provide 'my-latex)
;;; my-latex.el ends here


