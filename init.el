;; Add MELPA to package-archives. (https://www.emacswiki.org/emacs/MELPA)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Avoid loading the packages again after processing the init file. 
(setq package-enable-at-startup nil)

;; Load packages explicitly.
(package-initialize)

;; Set path to external file for use by the emacs customize system.
;; https://stackoverflow.com/a/5052111
(setq custom-file "~/.emacs.d/custom/emacs-customize.el")
(load custom-file)

;; Ensure all selected packages are installed.
;; https://stackoverflow.com/a/39891192/2974621
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Load custom scripts.
(add-to-list 'load-path "~/.emacs.d/custom/")
(require 'emacs-saves)
(require 'line-numbers)
(require 'move-line)
(require 'misc)

