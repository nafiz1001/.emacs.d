;; -*- lexical-binding: t -*-

(require 'package)
(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			   ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package emacs
  ;; scrolling
  :custom
  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 101)
  ;; completion
  (completion-styles '(basic partial-completion emacs22 substring))
  (completions-format 'one-column)
  (icomplete-mode t)
  ;; appearance
  (column-number-mode t)
  (custom-enabled-themes '(modus-vivendi))
  (global-display-line-numbers-mode t)
  (inhibit-startup-screen t)
  (size-indication-mode t)
  (tool-bar-mode nil)
  (visible-bell t)
  ;; other
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

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t))

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
  )

(use-package xref
  :custom
  (xref-search-program 'ripgrep))

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
  (apheleia-global-mode +1)
  (setf (alist-get 'prettier-json apheleia-formatters)
	'("apheleia-npx"
	  "prettier"
	  "--stdin-filepath"
	  filepath
	  (if (equal (file-name-nondirectory (apheleia-formatters-local-buffer-file-name)) "package.json")
	      "--parser=json-stringify"
	    "--parser=json")
	  (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package keycast
  :disabled
  :ensure t)
