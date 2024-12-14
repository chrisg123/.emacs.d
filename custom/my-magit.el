;;; my-magit.el --- Magit customization
;;; Commentary:

;;; Code:
(require 'magit)

(defun my-magit-setup-faces ()
  "Customize faces for Magit."

  (set-face-attribute 'magit-diff-base nil
                      :foreground "#aaaa11"
                      :background 'unspecified
                      :inherit nil)
  
  (set-face-attribute 'magit-diff-added nil
                      :foreground "#22aa22"
                      :background 'unspecified
                      :inherit nil)

  (set-face-attribute 'magit-diff-added-highlight nil
                      :foreground "#22aa22"
                      :background 'unspecified
                      :weight 'bold
                      :inherit nil)

  (set-face-attribute 'magit-diff-hunk-heading nil
                      :foreground "grey30"
                      :background 'unspecified
                      :inherit nil)

  (set-face-attribute 'magit-diff-removed nil
                      :foreground "#aa2222"
                      :background 'unspecified
                      :extend t
                      :inherit nil)

  (set-face-attribute 'magit-diff-removed-highlight nil
                      :foreground "#aa2222"
                      :background 'unspecified
                      :weight 'bold
                      :extend t
                      :inherit nil)

  (set-face-attribute 'magit-diff-context nil
                      :foreground "grey40"
                      :background 'unspecified
                      :extend t
                      :inherit nil)

  (set-face-attribute 'magit-diff-context-highlight nil
                      :foreground "grey40"
                      :background 'unspecified
                      :weight 'bold
                      :extend t
                      :inherit nil)
)

(with-eval-after-load 'magit
  (my-magit-setup-faces))

(provide 'my-magit)
;;; my-magit.el ends here
