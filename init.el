;;; init.el --- My init.el.
;;; Commentary:
;;; Code:

(require 'package)

;; Add MELPA to package-archives. (https://www.emacswiki.org/emacs/MELPA)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Avoid loading the packages again after processing the init file.
(setq package-enable-at-startup nil)

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
(require 'my-flycheck)
(require 'emacs-customize)
(require 'emacs-saves)
(require 'emacs-modes)
(require 'emacs-keys)
(require 'move-line)
(require 'xclip)
(require 'my-line-numbers)
(require 'my-helm)
(require 'my-org)
(require 'misc)

;;; init.el ends here
