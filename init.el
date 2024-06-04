;; -*- lexical-binding: t -*-

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package no-littering
  :vc (:fetcher github :repo emacscollective/no-littering)
  :config
  (no-littering-theme-backups))

(use-package emacs
  :custom
  (column-number-mode t)
  (completion-styles '(basic partial-completion emacs22 substring))
  (completions-format 'one-column)
  (custom-enabled-themes '(modus-vivendi))
  (electric-pair-mode t)
  (global-display-line-numbers-mode t)
  (icomplete-mode t)
  (inhibit-startup-screen t)
  (mouse-wheel-progressive-speed nil)
  (proced-show-remote-processes t)
  (scroll-conservatively 101)
  (size-indication-mode t)
  (tab-bar-mode t)
  (tool-bar-mode nil)
  (visible-bell t)
  :config
  (global-auto-revert-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  ;; (setq completion-ignore-case t) TODO: figure out how to make project-find-file case insensitive
  )

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
