;;; misc.el --- Miscellaneous Emacs settings
;;; commentary:
;; A place for miscellaneous Emacs settings.

;;; Code:

;; display the current column number
(setq column-number-mode t)

(global-flycheck-mode)

(semantic-mode 1)

;; Disable the splash screen
(setq inhibit-splash-screen t)

(defun is-gnu-screen ()
  "Check if gnu screen is running."
  (cond ((getenv "STY") t)))

(if 1 ;; used to use is-gnu-screen but set to always disable italic
    ;; since it caused issues when ssh'd into a remote machine
    ;; while using gnu-screen localy
    ;; Disable italic. Does not play well with gnu screen.
    (progn (set-face-italic 'font-lock-comment-face nil)
	   (set-face-italic 'font-lock-doc-face nil)
	   (set-face-italic 'font-lock-preprocessor-face nil)))

;; remove background color so transparency works.
;; TODO: maybe check if compositing is enabled first.
(set-background-color nil)

;; ANSI Color https://stackoverflow.com/a/23382008/2974621
(require 'ansi-color)
(defun display-ansi-colors ()
  "Display ANSI color codes."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq scroll-conservatively most-positive-fixnum)

(delete-selection-mode t)

(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . sh-mode))

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(push '("\\.\\(?:frm\\|\\(?:ba\\|cl\\|vb\\)s\\)\\'" . visual-basic-mode)
      auto-mode-alist)

(defvar visual-basic-mode-indent)
;;(defvar visual-basic-ide-pathname)
;;(setq visual-basic-ide-pathname "/bin/bash -c VB.exe")
(add-hook 'visual-basic-mode-hook ()
	  (setq visual-basic-mode-indent 4)
	  (setq-default indent-tabs-mode nil))

(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq eldoc-idle-delay 1)

(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(show-paren-mode t)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'misc)
;;; misc.el ends here
