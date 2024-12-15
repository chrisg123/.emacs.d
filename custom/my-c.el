;;; my-c.el --- C customization
;;; Commentary:

;;; Code:
(defvar c-mode-base-map)
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'semantic)
(require 'flycheck)
(require 'glsl-mode)
(require 'mmm-mode)

(defun my-emsdk-setup ()
  "."
  (let
      ((emsdk-path (getenv "EMSDK"))
       )
    (when emsdk-path
      (semantic-add-system-include
       (concat emsdk-path "/upstream/emscripten/cache/sysroot/include"))
      (setq flycheck-checker 'c/c++-emscripten)


      (put 'glsl-mode 'derived-mode-parent 'prog-mode)

      (mmm-add-classes
       '((embedded-js
          :submode js-mode
          :face mmm-declaration-submode-face
          :front "EM_ASM(.*\n?.*{"
          :back "},.*\n?.*);")
         ))

      (setq mmm-submode-decoration-level 0)
      (mmm-mode 1)
      )))

(add-hook
 'c-mode-common-hook
 (lambda()
   (semantic-mode 1)
   (tree-sitter-hl-mode)
   (setq-default flycheck-checker 'c/c++-gcc)
   (define-key c-mode-base-map
     (kbd "M-.") 'semantic-ia-fast-jump)
   (my-emsdk-setup)
   ))


(provide 'my-c)
;;; my-c.el ends here
