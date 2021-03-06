;;; my-company-mode.el --- Company mode customization
;;; Commentary:

;;; Code:

(add-hook 'after-init-hook 'global-company-mode)

;; Ref: https://www.monolune.com/configuring-company-mode-in-emacs/

(defvar company-idle-delay)
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 1)

;; Show suggestions after entering one character.
(defvar company-minimum-prefix-length 1)

;; Wrap suggestion list
(defvar company-selection-wrap-around)
(setq company-selection-wrap-around t)

(provide 'my-company-mode)
;;; my-company-mode.el ends here
