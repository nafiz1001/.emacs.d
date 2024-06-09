;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;; scrolling
(use-package emacs
  :custom
  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 101))

;; completion
(use-package emacs
  :custom
  (completion-styles '(basic partial-completion emacs22 substring))
  (completions-format 'one-column)
  (icomplete-mode t))

;; appearance
(use-package emacs
  :custom
  (column-number-mode t)
  (custom-enabled-themes '(modus-vivendi))
  (global-display-line-numbers-mode t)
  (inhibit-startup-screen t)
  (size-indication-mode t)
  (tool-bar-mode nil)
  (visible-bell t))

;; other
(use-package emacs
  :custom
  (electric-pair-mode t)
  :config
  (global-auto-revert-mode)
  (xterm-mouse-mode)
  ;; (setq completion-ignore-case t) TODO: figure out how to make project-find-file case insensitive
  )

(use-package tab-bar
  :custom
  (tab-bar-mode t))

(use-package proced
  :custom
  (proced-show-remote-processes t))

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package windmove
  :bind (("C-x <left>"  . windmove-left)
	 ("C-x <right>" . windmove-right)
	 ("C-x <up>"    . windmove-up)
	 ("C-x <down>"  . windmove-down)))

(use-package org
  :custom
  (org-directory "~/Documents/Org")
  :config
  ;; TODO: make org-mode-hook initial functions not lambda
  (cl-pushnew 'visual-line-mode org-mode-hook))

(use-package eglot
  :after (treesit)
  :config
  (add-to-list 'eglot-server-programs `(((js-mode :language-id "javascript")
                                         (js-ts-mode :language-id "javascript")
					 (tsx-ts-mode  :language-id "typescriptreact")
					 (typescript-ts-mode :language-id "typescript")
					 (typescript-mode  :language-id "typescript"))
					.
					("typescript-language-server" "--stdio"))))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package magit
  :ensure t)

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))
