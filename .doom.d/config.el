;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; settings
(setq scroll-margin 4)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan Denoux"
      user-mail-address "ryanpdenoux@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; vterm
(after! vterm
  (set-popup-rule! "*doom:vterm-popup:main"
    :size 0.40 :select t :quit 'other :ttl nil :side 'right))

;; projectile
(after! projectile
  (setq projectile-switch-project-action 'magit-status))

;; lsp
(after! lsp
  (setq lsp-lens-mode t)
  (setq lsp-ui-doc-position 'top
        lsp-ui-peek-enable t))

(setq flycheck-python-pylint-executable "pylint")
(use-package! lsp-pyright
  :config (setq lsp-clients-python-command "pyright")
  :hook python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp)))

(use-package! bash-language-server
  :hook (sh-mode . lsp))

(after! (python cc)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(after! python
  (display-fill-column-indicator-mode
   (setq display-fill-column-indicator-column 120))
  (setq poetry-tracking-strategy 'projectile)
  (map! :map python-mode-map
   :localleader
   :desc "Poetry tracking mode" "c" #'poetry-tracking-mode
   :desc "Restart LSP workspace" "r" #'lsp-workspace-restart
   :desc "Workon/off the Poetry venv" "w" #'poetry-venv-toggle
   :desc "Poetry menu" "p" #'poetry))

(use-package! sublimity)

(use-package! sublimity-attractive
  :config (setq sublimity-attractive-centering-width 132))
