;; set backup settings

(unless (file-accessible-directory-p "~/.emacs-saves")
  (make-directory "~/.emacs-saves"))
(setq backup-directory-alist '(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(provide 'emacs-saves)
