;;; my-flycheck.el --- My flycheck customization
;;; Commentary:
;;; Code:
(defvar flycheck-emacs-lisp-load-path)
(setq flycheck-emacs-lisp-load-path 'inherit)

(add-hook 'flycheck-error-list-mode-hook
          (lambda ()
            (setq tabulated-list-format '[("File" 6)
                                          ("Line" 5 flycheck-error-list-entry-< :right-align t)
                                          ("Col" 3 nil :right-align t)
                                          ("Level" 8 flycheck-error-list-entry-level-<)
                                          ("ID" 30 t)
                                          (#("Message (Checker)" 0 9
                                             (face default)
                                             9 16
                                             (face flycheck-error-list-checker-name)
                                             16 17
                                             (face default))
                                           0 t)])))

(provide 'my-flycheck)
;;; my-flycheck ends here
