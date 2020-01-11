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
 '(global-semantic-idle-scheduler-mode t)
 '(global-semanticdb-minor-mode t)
 '(package-selected-packages
   (quote
    (flycheck-gradle gradle-mode company-lsp kotlin-mode lsp-mode markdown-mode format-all android-mode flycheck-kotlin cmake-mode geben-helm-projectile geben highlight-indent-guides company-nginx yasnippet-snippets helm-c-yasnippet indent-tools yaml-mode helm-projectile ac-php company-php nginx-mode hindent ac-haskell-process haskell-snippets flycheck-ghcmod company-ghc xah-math-input web-mode tangotango-theme php-auto-yasnippets helm flycheck boron-theme)))
 '(yaml-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(diff-header ((t (:background "brightblack"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(helm-selection ((t (:background "color-238" :distant-foreground "black"))))
 '(helm-visible-mark ((t (:background "color-19"))))
 '(org-document-title ((t (:foreground "brightyellow" :weight bold))))
 '(pulse-highlight-start-face ((t (:background "color-19"))))
 '(semantic-decoration-on-fileless-includes ((t (:background "#f0fdf0" :foreground "black"))))
 '(semantic-decoration-on-unparsed-includes ((t (:background "#ffff55" :foreground "black"))))
 '(semantic-highlight-func-current-tag-face ((t (:background "color-19" :foreground "white")))))
