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
 '(custom-enabled-themes '(manoj-dark))
 '(elpy-syntax-check-command "pylint")
 '(global-semantic-idle-scheduler-mode t)
 '(global-semanticdb-minor-mode t)
 '(package-selected-packages
   '(jedi elpy lsp-ui linum-relative rust-mode flycheck-pycheckers python-mode xah-math-input multiple-cursors org-projectile flycheck-kotlin yaml-mode auto-complete company ghc groovy-mode magit company-lsp flycheck-gradle gradle-mode kotlin-mode markdown-mode format-all android-mode cmake-mode geben-helm-projectile geben highlight-indent-guides company-nginx helm-c-yasnippet ac-php company-php nginx-mode hindent ac-haskell-process haskell-snippets flycheck-ghcmod company-ghc helm))
 '(safe-local-variable-values '((c-indent-level . 4)))
 '(yaml-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(diff-header ((t (:background "brightblack"))))
 '(ediff-even-diff-A ((t (:extend t :background "color-245"))))
 '(ediff-even-diff-B ((t (:extend t :background "brightblack"))))
 '(ediff-odd-diff-A ((t (:extend t :background "brightblack"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(helm-selection ((t (:background "color-238" :distant-foreground "black"))))
 '(helm-visible-mark ((t (:background "color-19"))))
 '(highlight-indentation-face ((t (:inherit nil))))
 '(lsp-ui-peek-list ((t (:background "brightblack"))))
 '(lsp-ui-peek-peek ((t (:background "brightblack"))))
 '(magit-diff-added ((t (:foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:foreground "#22aa22" :weight bold))))
 '(magit-diff-base ((t (:foreground "#aaaa11"))))
 '(magit-diff-base-highlight ((t (:underline nil))))
 '(magit-diff-context-highlight ((t (:foreground "grey50"))))
 '(magit-diff-hunk-heading ((t (:foreground "grey30"))))
 '(magit-diff-hunk-heading-highlight ((t (:foreground "grey30" :weight bold))))
 '(magit-diff-removed-highlight ((t (:foreground "#aa2222"))))
 '(magit-section-highlight ((t (:underline nil :weight bold))))
 '(mode-line ((t (:background "grey30" :foreground "grey80" :box 1 :height 0.9))))
 '(mode-line-inactive ((t (:background "black" :foreground "grey80" :box 1 :weight light :height 0.9))))
 '(org-document-title ((t (:foreground "brightyellow" :weight bold))))
 '(pulse-highlight-start-face ((t (:background "color-19"))))
 '(semantic-decoration-on-fileless-includes ((t (:background "#f0fdf0" :foreground "black"))))
 '(semantic-decoration-on-unparsed-includes ((t (:background "#ffff55" :foreground "black"))))
 '(semantic-highlight-func-current-tag-face ((t (:weight bold))))
 '(vbnet-funcall-face ((t (:foreground "brightcyan"))))
 '(vbnet-namespace-face ((t (:foreground "brightmagenta"))))
 '(whitespace-space ((t (:foreground "#262626"))))
 '(whitespace-tab ((t (:foreground "#636363")))))
