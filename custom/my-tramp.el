;;; my-tramp.el --- Miscellaneous Emacs settings
;;; commentary:
;; A place for miscellaneous Emacs settings.

;;; Code:

(defvar tramp-default-method)
(defvar tramp-verbose)

(setq tramp-default-method "ssh")
(setq tramp-verbose 6)

(connection-local-set-profile-variables
 'remote-without-auth-sources '((auth-sources . nil)))

(provide 'my-tramp)
;;; my-tramp.el ends here
