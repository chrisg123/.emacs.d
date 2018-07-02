;;; misc.el --- Miscellaneous Emacs settings
;;; Commentary:
;; A place for miscellaneous Emacs settings.

;;; Code:

;; display the current column number
(setq column-number-mode t)

(global-flycheck-mode)

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; ANSI Color https://stackoverflow.com/a/23382008/2974621
(require 'ansi-color)
(defun display-ansi-colors ()
  "Display ANSI color codes."
  (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'misc)
;;; misc.el ends here
