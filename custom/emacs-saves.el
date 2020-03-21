;;; emacs-saves.el --- Emacs auto save settings
;;; Commentary:
;;; Code:

(defconst my-saves "~/.emacs-saves/")
(defconst auto-save-prefix ".auto-saves-")

(unless (file-accessible-directory-p my-saves)
  (make-directory my-saves))

(setq backup-directory-alist `(("." . ,my-saves)))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-list-file-prefix (concat my-saves auto-save-prefix))
(setq auto-save-file-name-transforms `((".*" ,my-saves t)))

(provide 'emacs-saves)

;;; emacs-saves.el ends here
