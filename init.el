;;; Vanilla Init

(setq inhibit-startup-message t)
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; prevent number lines in shell/term
;; https://github.com/daviwil/emacs-from-scratch/blob/d23348b4a52dde97f4f7cbcd66a519b5fd0a143c/init.el#L82-L88
(dolist (mode '(term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq native-comp-async-report-warnings-errors nil)

;; improve LSP performance
(setq gc-cons-threshold (* 1024 1024))
(setq read-process-output-max (* 1024 1024))

;;; Package System Setup

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;; Visual Packages

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :init (load-theme 'doom-acario-dark t)
  :config
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-16")
  ;; https://emacs.stackexchange.com/a/1062
  (let ((faces '(mode-line
		 mode-line-buffer-id
		 mode-line-emphasis
		 mode-line-highlight
		 mode-line-inactive)))
    (mapc
     (lambda
       (face)
       (set-face-attribute face nil :font "DejaVu Sans Mono-10"))
     faces)))

(use-package nyan-mode
  :config (nyan-mode 1))

;;; Behavioural Packages

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :custom (evil-collection-setup-minibuffer t))

(use-package evil-nerd-commenter
  :after evil
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(use-package magit :commands magit-status)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; install libtool and libtool-bin
(use-package vterm
  :commands vterm
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 1000))

(defun bash (buffer-name)
  (interactive "sbuffer name: ")
  (vterm "/bin/bash")
  (rename-buffer buffer-name t))

;; nice text completion when typing
(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; nice search minibuffer
(use-package ivy
  :config
  (ivy-mode 1))

;; project interaction (search files, compile, etc.)
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

;;; flycheck

(use-package flycheck
  :init (global-flycheck-mode))

;;; LSP and DAP

(use-package lsp-mode
  :after company
  :hook
  (lsp-mode . company-mode)
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (setq lsp-ui-doc-position 'bottom)
  :commands lsp-ui-mode)

(use-package dap-mode)

;;; AUCTeX

(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;;; OCaml

(use-package tuareg)

(let ((opam-share
       (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path
		 (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend))))
