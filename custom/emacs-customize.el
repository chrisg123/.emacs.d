;;; emacs-customize.el --- Emacs customize
;;; Commentary:
;; For use by the Emacs customize system.

;;; Code:

;; Set path to this external file for use by the emacs customize system.
;; https://stackoverflow.com/a/5052111
(setq custom-file "~/.emacs.d/custom/emacs-customize.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (manoj-dark)))
 '(package-selected-packages (quote (geben seq helm flycheck nlinum web-mode php-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'emacs-customize)

;;; emacs-customize.el ends here
