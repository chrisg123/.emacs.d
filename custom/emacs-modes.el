;;; emacs-modes.el --- Emacs mode settings
;;; Commentary:
;; An aggregate of Emacs mode settings

;;; Code:

;; Make web mode the default mode for certain file extensions
(add-to-list 'auto-mode-alist '("\\.html\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.proj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.vbproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.vcxproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.svg\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . xml-mode))

;; ANSI colors for .log files https://stackoverflow.com/a/23382008/2974621
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;; conf-mode
(add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.*rc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/yapf/style\\'" . conf-mode))

(provide 'emacs-modes)

;;; emacs-modes.el ends here
