;;; emacs-customize.el --- Emacs customize
;;; Commentary:
;; For use by the Emacs customize system.

;;; Code:

;; Set path to this external file for use by the emacs customize system.
;; https://stackoverflow.com/a/5052111
(setq custom-file "~/.emacs.d/custom/emacs-customize.el")




(provide 'emacs-customize)
;;; emacs-customize.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(manoj-dark))
 '(elpy-syntax-check-command "pylint")
 '(format-all-default-formatters
   '(("Assembly" asmfmt)
     ("ATS" atsfmt)
     ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex)
     ("C" clang-format)
     ("C#" clang-format)
     ("C++" clang-format)
     ("Cabal Config" cabal-fmt)
     ("Clojure" zprint)
     ("CMake" cmake-format)
     ("Crystal" crystal)
     ("CSS" prettier)
     ("Cuda" clang-format)
     ("D" dfmt)
     ("Dart" dart-format)
     ("Dhall" dhall)
     ("Dockerfile" dockfmt)
     ("Elixir" mix-format)
     ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp)
     ("F#" fantomas)
     ("Fish" fish-indent)
     ("Fortran Free Form" fprettify)
     ("GLSL" clang-format)
     ("Go" gofmt)
     ("GraphQL" prettier)
     ("Haskell" brittany)
     ("HTML" html-tidy)
     ("Java" clang-format)
     ("JavaScript" prettier)
     ("JSON" prettier)
     ("JSON5" prettier)
     ("Jsonnet" jsonnetfmt)
     ("JSX" prettier)
     ("Kotlin" ktlint)
     ("LaTeX" latexindent)
     ("Less" prettier)
     ("Literate Haskell" brittany)
     ("Lua" lua-fmt)
     ("Markdown" prettier)
     ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format)
     ("OCaml" ocp-indent)
     ("Perl" perltidy)
     ("PHP" prettier)
     ("Protocol Buffer" clang-format)
     ("PureScript" purty)
     ("Python" yapf)
     ("R" styler)
     ("Reason" bsrefmt)
     ("ReScript" rescript)
     ("Ruby" rufo)
     ("Rust" rustfmt)
     ("Scala" scalafmt)
     ("SCSS" prettier)
     ("Shell" shfmt)
     ("Solidity" prettier)
     ("SQL" sqlformat)
     ("Svelte" prettier)
     ("Swift" swiftformat)
     ("Terraform" terraform-fmt)
     ("TOML" prettier)
     ("TSX" prettier)
     ("TypeScript" prettier)
     ("V" v-fmt)
     ("Verilog" istyle-verilog)
     ("Vue" prettier)
     ("XML" html-tidy)
     ("YAML" prettier)
     ("_Angular" prettier)
     ("_Flow" prettier)
     ("_Gleam" gleam)
     ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt)
     ("_Snakemake" snakefmt)))
 '(global-semantic-idle-scheduler-mode t)
 '(global-semanticdb-minor-mode t)
 '(org-agenda-loop-over-headlines-in-active-region nil)
 '(package-selected-packages
   '(cmake-mode tree-sitter-langs yasnippet helm-pydoc pydoc string-inflection web-mode json-mode 0blayout vterm transpose-frame yaml-mode geben-helm-projectile highlight-indent-guides xterm-color projectile format-all elpy pyvenv lsp-sourcekit swift-mode kotlin-mode haskell-mode hindent mmm-mode glsl-mode helm ob-swift ob-kotlin lsp-ui lsp-mode magit tree-sitter flycheck gnu-elpa-keyring-update))
 '(safe-local-variable-values
   '((whitespace-style face lines indentation:space)
     (eval unless
           (featurep 'llbuild-project-settings)
           (message "loading 'llbuild-project-settings")
           (add-to-list 'load-path
                        (concat
                         (let
                             ((dlff
                               (dir-locals-find-file default-directory)))
                           (if
                               (listp dlff)
                               (car dlff)
                             (file-name-directory dlff)))
                         "utils/emacs")
                        :append)
           (require 'llbuild-project-settings))
     (eval add-hook 'prog-mode-hook
           (lambda nil
             (whitespace-mode 1))
           (not :APPEND)
           :BUFFER-LOCAL)
     (eval let*
           ((x
             (dir-locals-find-file default-directory))
            (this-directory
             (if
                 (listp x)
                 (car x)
               (file-name-directory x))))
           (unless
               (or
                (featurep 'swift-project-settings)
                (and
                 (fboundp 'tramp-tramp-file-p)
                 (tramp-tramp-file-p this-directory)))
             (add-to-list 'load-path
                          (concat this-directory "utils")
                          :append)
             (let
                 ((swift-project-directory this-directory))
               (require 'swift-project-settings)))
           (set
            (make-local-variable 'swift-project-directory)
            this-directory))
     (org-babel-noweb-wrap-end . "»")
     (org-babel-noweb-wrap-start . "«")
     (c-indent-level . 4)))
 '(yaml-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "WhiteSmoke" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(diff-header ((t (:background "brightblack"))))
 '(diff-refine-added ((t (:inherit diff-refine-changed :background "color-22"))))
 '(diff-refine-removed ((t (:inherit diff-refine-changed :background "brightblack"))))
 '(ediff-current-diff-A ((t (:extend t :background "red" :foreground "white"))))
 '(ediff-current-diff-B ((t (:extend t :background "color-22" :foreground "black"))))
 '(ediff-even-diff-A ((t (:extend t :background "color-237"))))
 '(ediff-even-diff-B ((t (:extend t :background "brightblack"))))
 '(ediff-odd-diff-A ((t (:extend t :background "brightblack" :foreground "white"))))
 '(ediff-odd-diff-B ((t (:background "color-17"))))
 '(font-lock-builtin-face ((t (:foreground "brightblue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "brightblack"))))
 '(font-lock-comment-face ((t (:foreground "brightblack" :slant normal))))
 '(font-lock-keyword-face ((t (:foreground "brightblue"))))
 '(font-lock-preprocessor-face ((t (:foreground "color-24" :slant normal))))
 '(font-lock-string-face ((t (:foreground "color-173"))))
 '(font-lock-type-face ((t (:foreground "color-36"))))
 '(helm-selection ((t (:background "color-238" :distant-foreground "black"))))
 '(helm-visible-mark ((t (:background "color-19"))))
 '(highlight-indentation-face ((t (:inherit nil))))
 '(lsp-ui-doc-background ((t (:background "color-17"))))
 '(lsp-ui-peek-list ((t (:background "brightblack"))))
 '(lsp-ui-peek-peek ((t (:background "brightblack"))))
 '(match ((t (:background "RoyalBlue" :foreground "white"))))
 '(mode-line ((t (:background "grey30" :foreground "grey80" :box 1 :height 0.9))))
 '(mode-line-inactive ((t (:background "black" :foreground "grey80" :box 1 :weight light :height 0.9))))
 '(org-code ((t (:foreground "brightblue"))))
 '(org-document-title ((t (:foreground "brightyellow" :weight bold))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "brightblack"))))
 '(pulse-highlight-start-face ((t (:background "color-19"))))
 '(region ((t (:extend t :background "color-17"))))
 '(semantic-decoration-on-fileless-includes ((t (:background "#f0fdf0" :foreground "black"))))
 '(semantic-decoration-on-unparsed-includes ((t (:background "#ffff55" :foreground "black"))))
 '(semantic-highlight-func-current-tag-face ((t (:weight bold))))
 '(smerge-lower ((t (:extend t :background "color-17"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "color-22"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "red"))))
 '(smerge-upper ((t (:extend t :background "red"))))
 '(table-cell ((t (:background "color-18" :foreground "gray90" :inverse-video nil))))
 '(tree-sitter-hl-face:function.call ((t (:inherit (link font-lock-function-name-face) :underline nil :weight normal))))
 '(vbnet-funcall-face ((t (:foreground "color-121"))))
 '(vbnet-namespace-face ((t (:foreground "brightmagenta"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-127"))))
 '(whitespace-hspace ((t (:background "brightblack" :foreground "color-237"))))
 '(whitespace-line ((t (:background "black" :foreground "violet"))))
 '(whitespace-space ((t (:foreground "#262626"))))
 '(whitespace-tab ((t (:foreground "#636363")))))

;;; emacs-customize.el ends here
