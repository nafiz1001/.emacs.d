;; -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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
  (no-littering-theme-backups))

(use-package emacs
  :ensure t
  ;; scrolling
  :custom
  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 101)
  ;; minibuffer
  (enable-recursive-minibuffers t)
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
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (delete-by-moving-to-trash t)
  :config
  (global-auto-revert-mode)
  (xterm-mouse-mode)
  (put 'upcase-region 'disabled nil))

(defun electric-pair-pairs-for-single-quote ()
  (interactive)
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?' . ?')))))

(use-package tab-bar
  :ensure t
  :custom
  (tab-bar-mode t))

(use-package proced
  :ensure t
  :custom
  (proced-show-remote-processes t))

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package wdired
  :ensure t
  :custom
  (wdired-allow-to-change-permissions t))

(use-package windmove
  :ensure t
  :bind (("C-x <left>"  . windmove-left)
	 ("C-x <right>" . windmove-right)
	 ("C-x <up>"    . windmove-up)
	 ("C-x <down>"  . windmove-down)))

(use-package org
  :ensure t
  :custom
  (org-directory "~/Documents/Org")
  ;;  (org-capture-templates '(("d"
  ;;			    "Diary/Journalling"
  ;;			    item
  ;;			    (file+headline (lambda () (expand-file-name (concat org-directory "/Diary/" (format-time-string "%Y-%m-%d.org"))))
  ;;					   (lambda () (format-time-string (org-time-stamp-format t t) (current-time))))
  ;;			    "- %?"
  ;;			    :empty-lines 0)))
  :config
  (add-hook 'org-mode-hook #'visual-line-mode))

(use-package xref
  :ensure t
  :custom
  (xref-search-program 'ripgrep))

(defun eglot-visual-line-mode ()
  "Fixes scrolling getting blocked by type hints so long they wrap at the edge of the buffer."
  (when (eglot-managed-p)
    (visual-line-mode)))
(use-package eglot
  :after (treesit)
  :config
  (add-to-list 'eglot-server-programs `(((js-mode :language-id "javascript")
                                         (js-ts-mode :language-id "javascript")
					 (tsx-ts-mode  :language-id "typescriptreact")
					 (typescript-ts-mode :language-id "typescript")
					 (typescript-mode  :language-id "typescript"))
					.
					("typescript-language-server" "--stdio")))
  (add-hook 'eglot-managed-mode-hook #'eglot-visual-line-mode))

(use-package tramp
  :ensure t
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
  :ensure t
  :config
  (editorconfig-mode 1))

;; End of Vanilla Configs

(use-package citre
  :ensure t
  :config
  (require 'citre-config))


(use-package orderless
  :disabled
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package mistty
  :ensure t)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package magit
  :ensure t)

(use-package diff-hl
  :vc (diff-hl :url "https://github.com/dgutov/diff-hl"
               :branch "master")
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (global-diff-hl-mode))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package nix-mode
  :disabled
  :ensure t
  :mode "\\.nix\\'")

(use-package rpm-spec-mode
  :ensure t
  :mode "\\.spec\\'")

(use-package keycast
  :disabled
  :ensure t)

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h x" . helpful-command)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function)))

(use-package copilot
  :ensure t)

(defun freezeman-backend ()
  (interactive)
  (ignore-error '(error "python manage.py runserver")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "python manage.py runserver")))
  (async-shell-command
   (concat "cd " (file-name-concat (project-root (project-current t)) "backend")
	   " && . ./env/bin/activate"
	   " && python manage.py runserver")
   "python manage.py runserver"))

(defun freezeman-frontend ()
  (interactive)
  (ignore-error '(error "No buffer named npm start")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "npm start")))
  (async-shell-command
   (concat "cd " (file-name-concat (project-root (project-current t)) "frontend")
	   " && npm start")
   "npm start"))
