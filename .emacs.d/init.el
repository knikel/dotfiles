;;; emacs.d -- Yup, emacs.d

;;; Commentary:
;; - Sane defaults
;; - Quality of life
;; - Elisp
;; - lsp-mode setup for TypeScript, Python, Rust
;; - Other modes

;;; Code:
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org-elpa" . "https://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Sane defaults
;;

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
;; Quality of life
;;;

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

(use-package diff-hl
  :ensure t
  :demand t
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode +1)
  (diff-hl-margin-mode))

(use-package dired
  :ensure nil
  :defer t
  :config
  (setq dired-listing-switches "-alh"))

(use-package doom-themes
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
    :family "Cascadia Code"
    :height (cond ((< (my-dpi) 110) 125) (t 125))
    :weight 'normal
    :width 'normal)
  (load-theme 'doom-solarized-light t))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package feebleline
  :ensure t
  :config
  (setq feebleline-msg-functions
        '((feebleline-line-number         :post "" :fmt "%5s")
          (feebleline-column-number       :pre ":" :fmt "%-2s")
          (feebleline-file-directory      :face feebleline-dir-face :post "")
          (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
          (feebleline-file-modified-star  :face font-lock-warning-face :post "")
          (feebleline-git-branch          :face feebleline-git-face :pre " : ")
          (feebleline-project-name        :align right)))
  (feebleline-mode 1))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package format-all :ensure t)

(use-package guru-mode
  :ensure t
  :demand t
  :config
  (guru-global-mode +1))

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

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package visual-fill-column
  :ensure t)

(use-package server
  :ensure nil
  :config
  (server-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq mode-require-final-newline nil)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode +1))

;;; lsp-mode configuration
;;

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config (require 'lsp-clients)
  :hook
  (python-mode . lsp)
  (rust-mode . lsp)
  (typescript-mode . lsp))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :defer t
  :commands company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends))

;; TypeScript
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (setq company-minimum-prefix-length 2))

;; Rust

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode
  :ensure t)

;;; Other modes and configuration

(use-package eldoc
  :ensure nil
  :config (global-eldoc-mode))

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

(use-package graphql-mode
  :ensure t)

(use-package ledger-mode
  :ensure t
  :mode "\\.dat\\'"
  :config
  (setq ledger-binary-path "hledger"
        ledger-mode-should-check-version nil
        ledger-highlight-xact-under-point nil)
  :custom (ledger-clear-whole-transactions t))

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

(use-package markdown-mode
  :after (visual-fill-column-mode)
  :ensure t
  :config
  :hook ((markdown-mode . visual-fill-column-mode)))

(use-package prog-mode
  :ensure nil
  :config
  (progn
    (add-hook 'prog-mode-hook 'company-mode)
    (add-to-list 'focus-out-hook (lambda () (save-some-buffers t nil)))
    (global-prettify-symbols-mode 1)
    (electric-pair-mode 1)))

(use-package sh-mode
  :ensure nil
  :defer t
  :config
  (progn
    (add-hook 'sh-mode-hook 'flycheck-mode)
    (add-hook 'sh-mode-hook 'linum-mode)
    (add-hook 'sh-mode-hook 'company-mode)))

(defvar current-time-format "%H:%M %p"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-time ()
  "Insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(global-set-key "\C-x\C-t" 'insert-current-time)
