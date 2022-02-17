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
   '(powershell multiple-cursors xterm-color ob-async csharp-mode sudo-edit string-inflection flycheck-rust flycheck-kotlin kotlin-mode lsp-ui swift-mode org jedi elpy linum-relative flycheck-pycheckers python-mode xah-math-input org-projectile yaml-mode auto-complete company ghc groovy-mode magit company-lsp flycheck-gradle gradle-mode markdown-mode format-all android-mode cmake-mode geben-helm-projectile geben highlight-indent-guides company-nginx helm-c-yasnippet ac-php company-php nginx-mode hindent ac-haskell-process haskell-snippets flycheck-ghcmod company-ghc helm))
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
 '(lsp-ui-doc-background ((t (:background "brightblack"))))
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
 '(org-code ((t (:foreground "brightblue"))))
 '(org-document-title ((t (:foreground "brightyellow" :weight bold))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "brightblack"))))
 '(pulse-highlight-start-face ((t (:background "color-19"))))
 '(semantic-decoration-on-fileless-includes ((t (:background "#f0fdf0" :foreground "black"))))
 '(semantic-decoration-on-unparsed-includes ((t (:background "#ffff55" :foreground "black"))))
 '(semantic-highlight-func-current-tag-face ((t (:weight bold))))
 '(smerge-lower ((t (:extend t :background "color-17"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "color-22"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "red"))))
 '(smerge-upper ((t (:extend t :background "red"))))
 '(vbnet-funcall-face ((t (:foreground "brightcyan"))))
 '(vbnet-namespace-face ((t (:foreground "brightmagenta"))))
 '(whitespace-line ((t (:background "black" :foreground "violet"))))
 '(whitespace-space ((t (:foreground "#262626"))))
 '(whitespace-tab ((t (:foreground "#636363")))))
