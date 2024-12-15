;;; my-flycheck.el --- My flycheck customization
;;; Commentary:
;;; Code:
(require 'flycheck)
(defvar flycheck-emacs-lisp-load-path)
(setq flycheck-emacs-lisp-load-path 'inherit)

(add-hook 'flycheck-error-list-mode-hook
          (lambda ()
            (setq tabulated-list-format '[("File" 6)
                                          ("Line" 5 flycheck-error-list-entry-< :right-align t)
                                          ("Col" 3 nil :right-align t)
                                          ("Level" 8 flycheck-error-list-entry-level-<)
                                          ("ID" 30 t)
                                          (#("Message (Checker)" 0 9
                                             (face default)
                                             9 16
                                             (face flycheck-error-list-checker-name)
                                             16 17
                                             (face default))
                                           0 t)])))

(with-eval-after-load 'flycheck
  (add-to-list 'flycheck-checkers 'c/c++-emscripten))

(defvar flycheck-emscripten-include-path '()
  "A list of include paths for the Emscripten checker.")

(flycheck-define-checker c/c++-emscripten
  "A C/C++ checker using Emscripten."
  :command ("emcc"
            "-Wall" "-Wextra"
            "-std=c++17"
            "-S"  ; Only run preprocesser and compilation steps
            "-o" "/dev/null"
            (option-list "-I" flycheck-emscripten-include-path)
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes (c-mode c++-mode))

(provide 'my-flycheck)
;;; my-flycheck ends here
