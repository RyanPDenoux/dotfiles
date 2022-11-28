;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; constants
(defconst my/org-directory "~/.local/org/")
(defconst my/org-agenda-directory (concat my/org-directory "agenda/"))
(defconst my/org-contact-location (concat my/org-agenda-directory "contacts.org"))
(defconst my/org-contact-capture-template
  "* %(org-contacts-template-name) %^g
:PROPERTIES:
:BIRTHDAY: %(org-read-date)
:EMAIL: %(org-contacts-template-email)
:END:")

;; settings
(setq scroll-margin 4)
(ispell-minor-mode 0)

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
(setq doom-font (font-spec :family "monospace" :size 12.0)
      doom-big-font (font-spec :family "monospace" :size 24.0)
      doom-variable-pitch-font (font-spec :family "sans" :size 14.0))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'gruvbox-dark-medium)

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

;; UI
(setq-default cursor-type 'box)
(setq mouse-wheel-scroll-amount '(1 ((shift . 1))))
(setq mouse-wheel-progressive-speed nil)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;       (assq-delete-all 'which-func-mode mode-line-misc-info))

(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))

(setq recenter-positions '(top middle bottom))

;; Themes
;; (use-package! modus-themes
;;   :bind ("<f5>" . modus-themes-toggle)
;;   :init
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs t
;;         modus-themes-subtle-line-numbers t
;;         modus-themes-variable-pitch-ui t)
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-vivendi))

;; functions
(defun my/os-switch-theme (appearance)
  "If the OS exposes some trigger, switch color themes"
  (mapc #'disable-theme (custom-available-themes))
  (pcase appearance
    ('light (load-theme 'doom-solarized-light t))
    ('dark (load-theme 'doom-solarized-dark t))))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (variable-pitch-mode 1))

(defun my/org-font-setup ()
  (dolist (face '((org-level-1 . 2.00)
                  (org-level-2 . 1.75)
                  (org-level-3 . 1.5)
                  (org-level-4 . 1.25)
                  (org-level-5 . 1.12)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "sans" :weight 'regular :height (cdr face)))
  (require 'org-indent)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil                 :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil               :inherit 'fixed-pitch)
  (set-face-attribute 'org-date nil                  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil                  :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil                 :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil              :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil       :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil             :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil              :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil               :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil  :inherit 'fixed-pitch))

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

;; avy
(use-package! avy
  :bind (("C-." . avy-goto-char)
         ("C->" . avy-goto-word-0))
  :custom
  (avy-background t))

(use-package! ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?k ?l))
  (aw-scope 'visible)
  (aw-dispatch-always t)
  :config
  (ace-window-display-mode 1))

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

;; sit on my face
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

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

(use-package! lsp-haskell
  :hook (haskell-mode . lsp))

(after! (python cc haskell)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; python
(after! python
  (display-fill-column-indicator-mode 1)
  (setq display-fill-column-indicator-column 120)
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

(add-to-list 'auto-mode-alist '("\\.service\\'"    . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'"      . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'"     . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'"      . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'"  . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'"      . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'"     . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'"       . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'"     . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'"    . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'"       . conf-unix-mode))

(use-package! paredit
  :hook (emacs-lisp-mode . enable-paredit-mode))

;; org
(use-package! org
  :commands (org-capture org-agenda)
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-read-date-force-compatible-dates nil
        org-agenda-files `(,(concat my/org-directory "agenda/")
                           ,(concat my/org-directory "gcal/")
                           ,(concat my/org-directory "todo.org"))
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-duration-format '(("h" . t) (special . 2))
        cursor-type 'bar)

  (my/org-font-setup))

(use-package! org-superstar-mode
  :hook (org-mode . org-superstar-mode))
(use-package! org-tree-slide
  :hook ((org-tree-slide-play . (lambda ()
                                  (setq text-scale-mode-amount 3)
                                  (org-display-inline-images)
                                  (text-scale-mode 1)))
         (org-tree-slide-stop . (lambda ()
                                  (text-scale-mode 0))))
  :custom
  (org-tree-slide-slide-in-effect t))

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

(defconst my/gcal-personal-org-file "~/.local/org/gcal/personal.org")
(defconst my/gcal-work-org-file "~/.local/org/gcal/work.org")
(defconst my/gcal-family-org-file "~/.local/org/gcal/family.org")

;; org-gcal
(use-package! org-gcal
  :config
  (setq org-gcal-client-id "806817204055-t3flmote5rf7ue57ddskeorahnjmn5e7.apps.googleusercontent.com"
        org-gcal-client-secret (password-store-copy "google.com/org-gcal")
        org-gcal-file-alist
        `(("ryanpdenoux@gmail.com" . ,my/gcal-personal-org-file)
          ("1418260bf6e2d909197b87cb0a1ce88499ee885bff153348ec20c62777f6ebeb@group.calendar.google.com" . ,my/gcal-work-org-file)
          ("2cea4fd0ecd153c55a4107767658c8e7a21c211ac6d3238e92e96fcc14b0a2c1@group.calendar.google.com" . ,my/gcal-family-org-file))
        org-gcal-recurring-events-mode 'nested)
  (add-hook 'org-agenda-mode-hook #'org-gcal-sync)
  (add-hook 'org-capture-after-finalize-hook #'org-gcal-sync)
  ; add some capture templates to really make this useful
  (dolist (template `(("c" "Calendar")
                      ("ca" "Personal" entry (file ,my/gcal-personal-org-file)
                       "* %?")
                      ("cw" "Work" entry (file ,my/gcal-work-org-file)
                       "* %?")
                      ("cf" "Family" entry (file ,my/gcal-family-org-file)
                       "* %?")))
    (add-to-list 'org-capture-templates template)))

;; org-caldav
;; (use-package! org-caldav)

;; org-contacts
(require 'diary-lib)
(use-package! org-contacts
  :after org
  :config
  (setq org-contacts-files `(,my/org-contact-location))
  (add-to-list 'org-capture-templates `("a" "New Contact" entry
                                        (file ,my/org-contact-location)
                                        ,my/org-contact-capture-template)))

;; visual-fill-column
(use-package! visual-fill-column
  :config
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t))

;; org-present
(defun my/org-present-start ()
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (setq-local face-remapping-alist '((default (:height 2.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 2.0) org-document-title)
                                     (org-code (:height 2.0) org-code)
                                     (org-verbatim (:height 2.0) org-verbatim)
                                     (org-block (:height 1.5) org-block)
                                     (org-block-begin-line (:height 0.75) org-block))))

(defun my/org-present-end ()
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
  (setq-local face-remapping-alist '((default variable-pitch default))))

(defun my/org-prepare-slides (buffer-name heading)
  "Setup slides to only show content as it appears"
  (org-overview)
  (org-fold-show-entry)
  (org-fold-show-children))

(use-package! org-present
  :config
  (add-hook 'org-present-mode-hook #'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook #'my/org-present-end)
  (add-hook 'org-present-after-navigate-functions #'my/org-prepare-slides))

;; mu4e
(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))

(set-email-account! "ryanpdenoux@gmail.com"
                    '((mu4e-sent-folder    . "/ryanpdenoux@gmail.com/[Gmail]/Sent Mail")
                      (mu4e-drafts-folder  . "/ryanpdenoux@gmail.com/[Gmail]/Drafts")
                      (mu4e-trash-folder   . "/ryanpdenoux@gmail.com/[Gmail]/Trash")
                      (mu4e-refile-folder  . "/ryanpdenoux@gmail.com/[Gmail]/All Mail")))

(set-email-account! "ryandenoux@me.com"
                    '((mu4e-sent-folder    . "/ryandenoux@me.com/Sent")
                      (mu4e-drafts-folder  . "/ryandenoux@me.com/Drafts")
                      (mu4e-trash-folder   . "/ryandenoux@me.com/Trash")
                      (mu4e-refile-folder  . "/ryandenoux@me.com/Archivel")))

;; password-store
(use-package! password-store)
