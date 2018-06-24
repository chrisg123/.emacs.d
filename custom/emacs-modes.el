;;; emacs-modes.el --- Emacs mode settings
;;; Commentary:
;; An aggregate of Emacs mode settings

;;; Code:

;; Make web mode the default mode for certain file extensions
(add-to-list 'auto-mode-alist '("\\.html\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'emacs-modes)

;;; emacs-modes.el ends here
