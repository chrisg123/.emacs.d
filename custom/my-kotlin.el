;;; my-kotlin.el --- Kotlin customization
;;; Commentary:

;;; Code:
(require 'kotlin-mode)
(require 'compile)

(defun compile-onramp-android()
  "Compile the onramp android app."
  (when (string-match-p "onramp/android/onramp" (pwd))
    (progn
      (setq compilation-read-command nil)
      (format "%srun.sh"
              (file-name-directory
               (directory-file-name
                (projectile-project-root))))
      )
    ))

(add-hook
 'kotlin-mode-hook
 (lambda()
   (setq-default kotlin-tab-width 4)
   (define-key kotlin-mode-map (kbd "C-c C-c") 'compile)
   (setq compile-command (compile-onramp-android))
   ))




(provide 'my-kotlin)

;;; my-kotlin.el ends here
