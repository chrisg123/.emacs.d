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
(package-install-selected-packages)

;; Load custom scripts.
(add-to-list 'load-path "~/.emacs.d/custom/")
(require 'my-indent)
(require 'my-flycheck)
(require 'my-semantic)
(require 'emacs-customize)
(require 'emacs-saves)
(require 'emacs-modes)
(require 'emacs-keys)
(require 'move-line)
(require 'xclip)
(require 'my-lsp)
(require 'my-line-numbers)
(require 'my-helm)
(require 'my-org)
(require 'my-c)
(require 'my-cxx)
(require 'my-haskell)
(require 'my-kotlin)
(require 'my-projectile)
(require 'misc)
(require 'my-yasnippet)
(require 'my-company-mode)
(require 'my-highlight-indent-guides)
(require 'geben-helm-projectile)
(require 'my-yaml)
(require 'my-multiple-cursors)
(require 'visual-basic-mode)
(require 'vbnet-mode)
(require 'hide-comnt)
(require 'ol-man)
(require 'copy-file-path)
(require 'my-tramp)
;;; init.el ends here

