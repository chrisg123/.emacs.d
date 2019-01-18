;;; emacs-customize.el --- Emacs customize
;;; Commentary:
;; For use by the Emacs customize system.

;;; Code:

;; Set path to this external file for use by the emacs customize system.
;; https://stackoverflow.com/a/5052111
(setq custom-file "~/.emacs.d/custom/emacs-customize.el")




(provide 'emacs-customize)
;;; emacs-customize.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (manoj-dark)))
 '(package-selected-packages
   (quote
    (ac-php company-php nginx-mode hindent ac-haskell-process haskell-snippets nlinum flycheck-ghcmod flycheck-haskell company-ghc xah-math-input web-mode tangotango-theme projectile php-auto-yasnippets helm haskell-mode geben flycheck boron-theme auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(diff-header ((t (:background "brightblack"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(helm-selection ((t (:background "color-238" :distant-foreground "black"))))
 '(org-document-title ((t (:foreground "brightyellow" :weight bold)))))
