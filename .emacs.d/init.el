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
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil)
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

(use-package avy :ensure t)

(use-package company :ensure t)

(use-package deft
  :ensure t
  :bind ("<f8>" . deft)
  :init (setq deft-directory "~/Dropbox/notes"
              deft-extensions '("md")
              deft-new-file-format "%Y%m%dT%H%M"))

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
    `("http://macwright.org/atom.xml"
      "http://bitemyapp.com/rss.xml"
      "http://degoes.net/feed.xml"
      "https://typeclasses.com/rss.xml"
      "http://calnewport.com/blog/feed?fmt=xml"
      "http://feeds.feedburner.com/HighScalability"
      ,(yt "UC5KbWmC93TBhinPLqh5j2kg")
      ,(yt "UCEBcDOjv-bhAmLavY71RMHA")
      ,(yt "UCEtohQeDqMSebi2yvLMUItg")
      ,(yt "UCJS9pqu9BzkAMNTmzNMNhvg")
      ,(yt "UCUgxpaK7ySR-z6AXA5-uDuw")
      ,(yt "UCmFs_X-O2fmq7Is1zMlxF1w")
      ,(yt "UCtmoqK-8MPiq4Vs_X2deDbg")
      ,(yt "UCuaSMQWO4ZG4EMSXRL0fldA")))
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

(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample))
  :defer t
  :ensure t
  :init
  (defun my-dpi ()
    (let* ((attrs (car (display-monitor-attributes-list)))
           (size (assoc 'mm-size attrs))
           (sizex (cadr size))
           (res (cdr (assoc 'geometry attrs)))
           (resx (- (caddr res) (car res)))
           dpi)
      (catch 'exit
        (unless sizex
          (throw 'exit 10))
        (when (> sizex 1000)
          (throw 'exit 10))
        (* (/ (float resx) sizex) 25.4))))
  :config
  (set-face-attribute 'default
    nil
    :family "M+ 1m"
    ;;:family "Noto Sans Mono"
    :height (cond ((< (my-dpi) 110) 140) (t 130))
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
        ledger-highlight-xact-under-point nil)
  :custom (ledger-clear-whole-transactions t))

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

(use-package visual-fill-column
  :ensure t)

(use-package markdown-mode
  :after (visual-fill-column-mode)
  :ensure t
  :config
  :hook ((markdown-mode . visual-fill-column-mode)))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-sync-dir "~/Dropbox/orgfiles")
  (setq org-hide-emphasis-markers t)
  (defun org-dir (path) (expand-file-name path org-sync-dir))
  :config
  (setq org-cycle-separator-lines 1)
  (setq org-agenda-files
	(mapcar 'org-dir
		'("inbox.org"
		  "todo.org"
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

  (setq org-refile-targets `((,(org-dir "todo.org") :maxlevel . 1)
			     (,(org-dir "someday.org") :level . 1)
			     (,(org-dir "tickler.org" ) :maxlevel . 2)))

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-switchb)

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(use-package prog-mode
  :ensure nil
  :config
  (progn
    (add-hook 'prog-mode-hook 'company-mode)
    (add-to-list 'focus-out-hook (lambda () (save-some-buffers t nil)))
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

(use-package server
  :ensure nil
  :config
  (server-mode))

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
         (typescript-mode . flycheck-mode)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package yasnippet
  :ensure t
  :config
  (setq mode-require-final-newline nil)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode +1))
