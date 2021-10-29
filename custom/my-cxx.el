;;; my-cxx.el --- C++ customization
;;; Commentary:

;;; Code:
(defvar c++-mode-map)
(add-hook
 'c++-mode-hook
 (lambda()
   (semantic-mode 1)
   (setq-default flycheck-checker 'c/c++-gcc)
   (setq c-hungry-delete-key nil)
   (define-key c++-mode-map
     (kbd "M-.") 'semantic-ia-fast-jump)))

(provide 'my-cxx)
;;; my-cxx.el ends here
