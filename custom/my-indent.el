;;; my-indent.el --- Emacs auto save settings
;;; Commentary:
;;; [[https://dougie.io/emacs/indentation/][ref]]
;;; [[http://ergoemacs.org/emacs/whitespace-mode.html][ref]]
;;; Code:

(defvar my-default-tab-width)
(setq my-default-tab-width 4)
(setq-default tab-width my-default-tab-width)
(setq-default fill-column 81)

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
(setq whitespace-style
      '(face spaces space-mark tabs tab-mark lines-tail trailing))

(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))

(custom-set-faces
 '(whitespace-space ((t (:foreground "#262626")))))

(setq-default indent-tabs-mode nil)

(setq electric-indent-mode nil)

(setq nobreak-char-display nil)

(global-whitespace-mode t)

(defun my:force-modes (rule-mode &rest modes)
    "switch on/off several modes depending of state of
    the controlling minor mode
  "
    (let ((rule-state (if rule-mode 1 -1)
                      ))
      (mapcar (lambda (k) (funcall k rule-state)) modes)
      )
    )

(defvar my:prev-whitespace-mode nil)
(make-variable-buffer-local 'my:prev-whitespace-mode)
(defvar my:prev-whitespace-pushed nil)
(make-variable-buffer-local 'my:prev-whitespace-pushed)

(defun my:push-whitespace (&rest skip)
  (if my:prev-whitespace-pushed () (progn
                                     (setq my:prev-whitespace-mode global-whitespace-mode)
                                     (setq my:prev-whitespace-pushed t)
                                     (my:force-modes nil 'global-whitespace-mode)
                                     ))
  )

(defun my:pop-whitespace (&rest skip)
  (if my:prev-whitespace-pushed (progn
                                  (setq my:prev-whitespace-pushed nil)
                                  (my:force-modes my:prev-whitespace-mode 'global-whitespace-mode)
                                  ))
  )

(add-hook 'company-completion-started-hook #'my:push-whitespace)
(add-hook 'company-completion-finished-hook  #'my:pop-whitespace)

(advice-add 'lsp-ui-doc--display :before #'my:push-whitespace)
(advice-add 'lsp-ui-doc--hide-frame :after #'my:pop-whitespace)

(provide 'my-indent)

;;; my-indent.el ends here
