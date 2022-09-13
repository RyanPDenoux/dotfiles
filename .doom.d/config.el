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
(setq doom-font (font-spec :family "monospace" :size 16)
      doom-big-font (font-spec :family "monospace" :size 32)
      doom-variable-pitch-font (font-spec :family "sans" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.local/org/")

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

;; defaults
(setq-default cursor-type 'bar)
(setq mouse-wheel-scroll-amount '(1 ((shift . 1))))
(setq mouse-wheel-progressive-speed nil)

(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))

;; builtins
(setq recenter-positions '(top middle bottom))

;; functions
(defun my/os-switch-theme (appearance)
  "If the os has something like an appearance, switch color themes"
  (mapc #'disable-theme (custom-available-themes))
  (pcase appearance
    ('light (load-theme 'doom-solarized-light t))
    ('dark (load-theme 'doom-solarized-dark t))))

(defun my/org-font-setup ()
  (dolist (face '((org-level-1 . 1.75)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.25)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "sans" :weight 'regular :height (cdr face)))
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun scroll-half-page-up ()
  "Scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))
(global-set-key (kbd "C-v") #'scroll-half-page-up)

(defun scroll-half-page-down ()
  "Scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))
(global-set-key (kbd "M-v") #'scroll-half-page-down)

;; which-key
(setq which-key-popup-type 'minibuffer)

;; completion company
(after! company
  (setq company-idle-delay 1))

;; :editor evil
(setq evil-split-window-below t
      evil-vsplit-window-nnright t)

;; format
(add-hook 'python-mode-hook #'format-all-mode)

;; modeline
(setq doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-height 25)

;; treemacs
(setq doom-themes-treemacs-theme "doom-colors")

;; vterm
(after! vterm
  (setq vterm-environment '("ZDOTDIR=/home/ryan/.config/zsh"))
  (set-popup-rule! "*doom:vterm-popup:main"
    :size 0.20 :select t :quit 'other :ttl nil :side 'bottom))

;; projectile
(after! projectile
  (setq projectile-switch-project-action 'magit-status))

;; haskell
(after! haskell
  (setq lsp-haskell-formatting-provider "brittany"))

;; clang
(setq lsp-clients-clangd-args '("-j=4"
                                "--clang-tidy"
                                "--completion-style=detailed"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; lsp
(after! lsp-mode
  (setq lsp-lens-mode t))
(after! lsp-ui
  (setq lsp-ui-doc-position 'top
        lsp-ui-peek-enable t))

(use-package! bash-language-server
  :hook (sh-mode . lsp))

(after! (python cc)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; python
(after! python
  (display-fill-column-indicator-mode
   (setq display-fill-column-indicator-column 120))
  (setq poetry-tracking-strategy 'projectile)
  (map! :map python-mode-map
   :localleader
   ;; :desc "Poetry tracking mode" "c" #'poetry-tracking-mode
   :desc "Restart LSP workspace" "r" #'lsp-workspace-restart
   :desc "Workon/off the Poetry venv" "w" #'poetry-venv-toggle
   :desc "Poetry menu" "p" #'poetry))

(use-package! lsp-pyright
  :config (setq lsp-clients-python-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(setq flycheck-python-pylint-executable "pylint")

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))

;; org
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)
(after! org
  :init
  (setq org-agenda-files '("~/.local/org/agenda/")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-duration-format '(("h" . t) (special . 2)))
  (my/org-font-setup))
(after! org-tree-slide
  (advice-remove 'org-tree-slide--display-tree-with-narrow
                 #'+org-present--hide-first-heading-maybe-a))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; mu4e
;; (setq +mu4e-backend 'offlineimap)
;; (after! mu4e
;;   (setq sendmail-program (executable-find "msmtp")
;;         send-mail-function #'smtpmail-send-it
;;         message-sendmail-f-is-evil t
;;         message-sendmail-extra-arguments '("--read-envelope-from")
;;         message-send-mail-function #'message-send-mail-with-sendmail))

;; packages
(use-package! sublimity)
(use-package! sublimity-attractive
  :config (setq sublimity-attractive-centering-width 132))
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package! paredit)
(add-hook! 'emacs-lisp-mode-hook #'enable-paredit-mode)
