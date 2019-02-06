
;;; my-highlight-indent-guides.el --- Highlight Indent Guides customization
;;; Commentary:

;;; Code:
(require 'highlight-indent-guides)
(setq highlight-indent-guides-auto-enabled nil)
(setq highlight-indent-guides-method 'column)

(set-face-background 'highlight-indent-guides-odd-face "color-232")
(set-face-background 'highlight-indent-guides-even-face "color-233")
(set-face-background 'highlight-indent-guides-character-face "dimgrey")

(add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)

(provide 'my-highlight-indent-guides)
;;; my-highlight-indent-guides.el ends here
