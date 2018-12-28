;; Package manager

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;;
;; Startup config
;;;

(progn
  (setq-default indent-tabs-mode nil)
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq package-enable-at-startup nil)
  (setq user-emacs-directory (file-name-directory user-init-file))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file) (load custom-file))
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (global-auto-revert-mode t)
  (setq ring-bell-function
        (lambda ()
          (let ((orig-fg (face-foreground 'mode-line)))
            (set-face-foreground 'mode-line "#F2804F")
            (run-with-idle-timer 0.1 nil
                                 (lambda (fg) (set-face-foreground 'mode-line fg))
                                 orig-fg))))
  (defalias 'yes-or-no-p 'y-or-n-p))

;;;
;; Packages, modes and scope customizations
;;;

(use-package better-defaults :ensure t)

(use-package company :ensure t)

(use-package dired
  :ensure nil
  :defer t
  :config
  (setq dired-listing-switches "-alh"))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package eldoc
  :ensure nil
  :config (global-eldoc-mode))

(use-package elfeed
  :ensure t
  :demand t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (defun yt (ch) (concat "https://www.youtube.com/feeds/videos.xml?channel_id=" ch))
  (setq elfeed-feeds
    `())
  (setq-default elfeed-search-filter "@7-days-ago +unread"))

(use-package elisp-mode
  :ensure nil
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package es-mode
  :ensure t
  :mode "\\.es$\\'")

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck :ensure t)

(use-package flycheck-ledger
  :ensure t
  :after (ledger-mode flycheck))

(use-package format-all :ensure t)

(use-package graphql-mode :ensure t)

(use-package intellij-theme
  :ensure t
  :config
  (set-face-attribute 'default
    nil
    :family "M+ 1mn"
    :height 130
    :weight 'normal
    :width 'normal))

(use-package diff-hl
  :ensure t
  :demand t
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode +1))

(use-package guru-mode
  :ensure t
  :demand t
  :config
  (guru-global-mode +1))

(use-package ledger-mode
  :ensure t
  :mode "\\.dat\\'"
  :config
  (setq ledger-binary-path "hledger"
        ledger-mode-should-check-version nil
        ledger-highlight-xact-under-point nil))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package makefile-mode
  :ensure nil
  :mode "\\Makefile\\'"
  :init
  (progn
    (font-lock-add-keywords
     'makefile-mode
     '(("define" . font-lock-keyword-face)
       ("endef" . font-lock-keyword-face)
       ("ifeq" . font-lock-keyword-face)
       ("ifneq" . font-lock-keyword-face)
       ("ifdef" . font-lock-keyword-face)
       ("ifndef" . font-lock-keyword-face)
       ("else" . font-lock-keyword-face)
       ("endif" . font-lock-keyword-face)))

    (defun makefile-mode-setup ()
      (setq whitespace-style '(face tab-mark trailing)))

    (add-hook 'makefile-mode-hook 'linum-mode)
    (add-hook 'makefile-mode-hook 'makefile-mode-setup)))

(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-sync-dir "~/Dropbox/orgfiles")
  (defun org-dir (path) (expand-file-name path org-sync-dir))
  :config
  (setq org-cycle-separator-lines 1)
  (setq org-agenda-files
	(mapcar 'org-dir
		'("inbox.org"
		  "master.org"
		  "tickler.org"
		  "habits.org")))

  (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                 (file+headline ,(org-dir "inbox.org") "Inbox")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline ,(org-dir "tickler.org") "Tickler")
                                 "* %i%? \n %U")
                                ("j" "Journal Entry" entry
                                 (file+datetree ,(org-dir "journal.org") "Journal")
                                 "* %U %?" :empty-lines 1)
                                ("n" "Note (for currently clocked task)" item
                                 (clock) "  - %U %?" :empty-lines 1)))

  (setq org-refile-targets `((,(org-dir "master.org") :maxlevel . 3)
			     (,(org-dir "someday.org") :level . 1)
			     (,(org-dir "tickler.org" ) :maxlevel . 2)))

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-switchb)

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))

(use-package prog-mode
  :ensure nil
  :config
  (progn
    (add-hook 'prog-mode-hook 'company-mode)
    (global-prettify-symbols-mode 1)
    (electric-pair-mode 1)))

(use-package projectile
  :ensure t
  :demand t
  :init
  (setq projectile-git-submodule-command "")
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package rainbow-delimiters :ensure t)

(use-package sh-mode
  :ensure nil
  :defer t
  :config
  (progn
    (add-hook 'sh-mode-hook 'flycheck-mode)
    (add-hook 'sh-mode-hook 'linum-mode)
    (add-hook 'sh-mode-hook 'company-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package yasnippet
  :ensure t
  :config
  (setq mode-require-final-newline nil)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode +1))
