;; -*- lexical-binding: t; -*-

;;; Vanilla Init

(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; https://github.com/daviwil/dotfiles/blob/0e034ebb688633082a3b0c201b0bc834a9821091/Emacs.org
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX
(setq vc-follow-symlinks t)
(setq truncate-lines t)

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

(defun my-configure-font ()
  "Configure font for first frame"

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
     faces))

  (if (daemonp)
      (remove-hook 'server-after-make-frame-hook #'my-configure-font)
    (remove-hook 'window-setup-hook #'my-configure-font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my-configure-font)
  (add-hook 'window-setup-hook #'my-configure-font))

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

(set-default-coding-systems 'utf-8)

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

(use-package no-littering
  :straight
  (no-littering
   :type git
   :host github
   :repo "emacscollective/no-littering"))

;;; Visual Packages

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-region '(bg-only no-extend))
  :config
  (load-theme 'modus-vivendi t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode)

(use-package nyan-mode
  :config (nyan-mode 1))

;;; Behavioural Packages

(use-package undo-fu)

(use-package evil
  :after undo-fu
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

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
  (setq vterm-max-scrollback 1000))

(defun new-term (buffer-name)
  (interactive "sbuffer name: ")
  (vterm vterm-shell)
  (rename-buffer buffer-name t))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package hl-todo
  :straight
  (hl-todo
   :type git
   :host github
   :repo "tarsius/hl-todo")
  :config
  (global-hl-todo-mode)
  :init
  (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert))

(use-package consult)

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package company-mode
  :hook (after-init . global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package projectile
  :config
  (projectile-mode +1))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t))

;;; Rust

(use-package rustic)

;;; AUCTeX

(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;;; OCaml

(use-package tuareg :commands tuareg-mode)

(let ((opam-share
       (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path
		 (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)))
