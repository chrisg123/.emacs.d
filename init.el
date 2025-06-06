;;; init.el --- My init.el.
;;; Commentary:
;;; Code:

;; Set encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(require 'package)

;; Add MELPA to package-archives. (https://www.emacswiki.org/emacs/MELPA)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Avoid loading the packages again after processing the init file.
(setq package-enable-at-startup nil)

;; https://emacs.stackexchange.com/a/51772
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Load packages explicitly.
(package-initialize)

;; Ensure all selected packages are installed.
;; https://stackoverflow.com/a/39891192/2974621
;; Note: (package-selected-packages) is declared in (custom-file).
(unless package-archive-contents
  (package-refresh-contents))
;;(package-install-selected-packages)

;; Load custom scripts.
(add-to-list 'load-path "~/.emacs.d/custom/")
(add-to-list 'load-path "~/.emacs.d/custom/ob-powershell")

(require 'emacs-saves)
(require 'emacs-modes)
(require 'emacs-keys)
(require 'emacs-customize)
(require 'my-indent)
(require 'my-flycheck)
(require 'my-semantic)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'my-magit)
(require 'move-line)
(require 'xclip)
(require 'format-all)
(require 'my-lsp)
(require 'my-line-numbers)
(require 'my-org)
(require 'my-helm)
(require 'my-c)
(require 'my-cxx)
(require 'my-haskell)
(require 'my-kotlin)
(require 'my-swift)
(require 'my-python)
(require 'my-projectile)
(require 'vbnet-mode)
(require 'my-xaml)
(require 'misc)
(require 'my-vbnet)
(require 'my-yasnippet)
(require 'my-company-mode)
(require 'my-highlight-indent-guides)
(require 'geben-helm-projectile)
(require 'my-yaml)
(require 'my-multiple-cursors)
(require 'visual-basic-mode)
(require 'hide-comnt)
(require 'ol-man)
(require 'copy-file-path)
(require 'transpose-frame)
(require 'my-tramp)
(require 'zpl-mode)
(require 'my-perl)
(require 'my-latex)
(require 'highlight)
(require 'my-markdown)
(require 'my-js)

(let ((gptel-path "~/.emacs.d/custom/gptel"))
  (when (file-directory-p gptel-path)
    (add-to-list 'load-path gptel-path)
    (require 'my-gptel)))

;;; init.el ends here
