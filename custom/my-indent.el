;;; my-indent.el --- Emacs auto save settings
;;; Commentary:
;;; [[https://dougie.io/emacs/indentation/][ref]]
;;; [[http://ergoemacs.org/emacs/whitespace-mode.html][ref]]
;;; Code:

(defvar my-default-tab-width)
(setq my-default-tab-width 4)
(setq-default tab-width my-default-tab-width)

(defun enable-tabs()
  "Enable tabs."
  (interactive)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t))

(defun disable-tabs()
  "Disable tabs."
  (interactive)
  (setq indent-tabs-mode nil))

(setq backward-delete-char-untabify-method 'hungry)

(defvar whitespace-style)
(setq whitespace-style '(face spaces space-mark tabs tab-mark trailing))

(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))

(custom-set-faces
 '(whitespace-space ((t (:foreground "#262626")))))

(global-whitespace-mode t)

(provide 'my-indent)

;;; my-indent.el ends here
