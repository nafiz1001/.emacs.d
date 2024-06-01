;; -*- lexical-binding: t -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(completion-styles '(basic partial-completion emacs22 substring))
 '(completions-format 'one-column)
 '(custom-enabled-themes '(modus-vivendi))
 '(electric-pair-mode t)
 '(global-display-line-numbers-mode t)
 '(icomplete-mode t)
 '(inhibit-startup-screen t)
 '(mouse-wheel-progressive-speed nil)
 '(org-directory "~/Documents/Org")
 '(package-selected-packages '(markdown-mode nix-mode no-littering))
 '(package-vc-selected-packages
   '((no-littering :vc-backend Git :url "https://github.com/emacscollective/no-littering")))
 '(proced-show-remote-processes t)
 '(scroll-conservatively 101)
 '(size-indication-mode t)
 '(tab-bar-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-auto-revert-mode)
(put 'dired-find-alternate-file 'disabled nil)

(package-install-selected-packages t)
(package-vc-install-selected-packages)

(use-package no-littering
  :config
  (no-littering-theme-backups))

(use-package org
  :config
  (cl-pushnew 'visual-line-mode org-mode-hook))

