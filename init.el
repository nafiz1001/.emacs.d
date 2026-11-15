;; -*- lexical-binding: t -*-

;;; Vanilla Configurations

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			   ("melpa" . "https://melpa.org/packages/")))

(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups))

(use-package emacs
  :ensure nil
  :custom
  ;; scrolling
  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 101)
  ;; echo
  (auto-revert-verbose nil)
  ;; minibuffer
  (enable-recursive-minibuffers t)
  ;; completion
  (completion-styles '(basic partial-completion emacs22 substring flex))
  (completions-sort 'historical)
  (fido-mode t)
  (global-completion-preview-mode t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
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
  (delete-by-moving-to-trash t)
  (savehist-mode t)
  (recentf-mode t)
  (save-place-mode t)
  (which-key-mode t)
  (repeat-mode t)
  (context-menu-mode t)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode)

  (xterm-mouse-mode)

  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  (global-so-long-mode))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-mode t))

(use-package proced
  :ensure nil
  :custom
  (proced-show-remote-processes t))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  :bind (:map dired-mode-map
              ("W" . (lambda () (interactive) (dired-copy-filename-as-kill 0))))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package wdired
  :ensure nil
  :custom
  (wdired-allow-to-change-permissions t))

(use-package windmove
  :ensure nil
  :bind (("C-x <left>"  . windmove-left)
	 ("C-x <right>" . windmove-right)
	 ("C-x <up>"    . windmove-up)
	 ("C-x <down>"  . windmove-down)))

(use-package org
  :ensure nil
  :custom
  (org-directory "~/Documents/Org")
  :config
  (add-hook 'org-mode-hook #'visual-line-mode))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep)
  :custom-face
  (xref-file-header ((t (:weight bold :underline t)))))

(use-package eglot
  :ensure nil
  :after (treesit)
  ;; Inlay hints are overlay strings. If a hint wraps at the window edge,
  ;; scrolling gets blocked. The display engine handles wrapped overlay
  ;; strings badly, see:
  ;; https://debbugs.gnu.org/20847
  ;; https://debbugs.gnu.org/25246
  ;; https://debbugs.gnu.org/67470
  :hook (eglot-managed-mode . visual-line-mode)
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :underline t
                      :weight 'bold))

(use-package tramp
  :ensure nil
  :custom
  (tramp-use-connection-share nil)
  :config
  ;; configurations for flatpak Emacs
  (if (not (assoc "toolbox" tramp-methods))
      (push
       (cons
	"toolbox"
	'((tramp-login-program "flatpak-spawn --host toolbox")
	  (tramp-login-args (("enter" "-c") ("%h")))
	  (tramp-remote-shell "/bin/sh")
	  (tramp-remote-shell-args ("-i") ("-c"))))
       tramp-methods)))

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1))

(defun artist-mode-setup ()
  (setq indent-tabs-mode nil))
(use-package artist-mode
  :ensure nil
  :hook (artist-mode . artist-mode-setup))

(defun unhighlight-all ()
  (interactive)
  (hi-lock-mode -1))

(defun copy-buffer-file-name ()
  (interactive)
  (kill-new (buffer-file-name)))

(defun copy-buffer-name ()
  (interactive)
  (kill-new (buffer-name)))

;;; External Configurations

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents   . 5)
                     (projects  . 10)
                     (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook))

;; dired font-lock
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

;; ctags
(use-package citre
  :ensure t
  :config
  (require 'citre-config))

(use-package mistty
  :ensure t)

(use-package treesit
  :ensure nil
  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'ask))

(use-package magit
  :ensure t)
(use-package project
  :ensure nil
  :after (magit)
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m) t)
  (define-key project-prefix-map "m" #'magit-project-status))

(use-package diff-hl
  :ensure t
  :vc (diff-hl :url "https://github.com/dgutov/diff-hl"
               :rev "8dfb5c27ea43b8c7ac1c008bb97090ad4f059285")
  ;; :hook defers loading, but global-diff-hl-mode must start at init.
  :demand t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :custom
  (diff-hl-disable-on-remote t)
  :config
  (global-diff-hl-mode))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode))

(use-package docker
  :ensure t)

(use-package vlf
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package hl-todo
  :ensure t
  :hook (prog-mode))

;;; Project Specific Configuration

;;;; Freezeman

(defconst freezeman-project-root "~/projects/c3g-workspace/freezeman/")

(defun freezeman-backend ()
  (interactive)
  (ignore-error '(error "python manage.py runserver")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "python manage.py runserver")))
  (async-shell-command
   (concat "cd " (file-name-concat freezeman-project-root "backend")
	   " && . ./.venv/bin/activate"
	   " && python manage.py runserver")
   "python manage.py runserver"))

(defun freezeman-frontend ()
  (interactive)
  (ignore-error '(error "No buffer named npm start")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "npm start")))
  (async-shell-command
   (concat "cd " (file-name-concat freezeman-project-root "frontend")
	   " && npm start")
   "npm start"))

(defun freezeman-shell ()
  (interactive)
  (let ((default-directory freezeman-project-root))
    (run-python (concat (file-name-concat freezeman-project-root "backend" ".venv" "bin" "python")
                        " "
                        (file-name-concat "backend" "manage.py")
                        " "
                        "shell")
                nil
                t)))
