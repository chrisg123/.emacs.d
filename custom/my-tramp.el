;;; my-tramp.el --- Miscellaneous Emacs settings
;;; commentary:
;; A place for miscellaneous Emacs settings.

;;; Code:
(require 'tramp)
(require 'emacs-saves)
(defvar tramp-default-method)
(defvar tramp-verbose)

(setq tramp-default-method "ssh")
(setq tramp-verbose 6)

(connection-local-set-profile-variables
 'remote-without-auth-sources '((auth-sources . nil)))

;; Turn off backup feature for remote files and stop tramp from saving to backup
;; directory.
;; https://www.gnu.org/software/tramp/#Auto_002dsave-File-Lock-and-Backup
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(setq tramp-auto-save-directory (concat my-saves "tramp/"))

(add-to-list 'tramp-methods
             '("ssh-no-pubkey"
               (tramp-login-program        "ssh")
               (tramp-login-args           (("-l" "%u") ("-p" "%p")
                                            ("-o" "PubkeyAuthentication=no")
                                            ("-e" "none") ("%h")))
               (tramp-async-args           (("-q")))
               (tramp-remote-shell         "/bin/sh")
               (tramp-remote-shell-args    ("-c"))))


(provide 'my-tramp)
;;; my-tramp.el ends here
