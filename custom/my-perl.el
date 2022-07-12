;;; my-perl.el --- Perl customization
;;; Commentary:

;;; Code:
(require 'perl-mode)
(add-hook 'perl-mode-hook
          (lambda()
            (define-key perl-mode-map (kbd "C-c C-c") 'executable-interpret)
            (helm-perldoc:setup)
            )
          )
(provide 'my-perl)

;;; my-perl.el ends here
