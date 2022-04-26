;;; Vanilla Init

(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

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

(load-theme 'modus-vivendi t)
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

(use-package nyan-mode
  :config (nyan-mode 1))

;;; Behavioural Packages

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
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
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 1000))

(defun bash (buffer-name)
  (interactive "sbuffer name: ")
  (vterm "/bin/bash")
  (rename-buffer buffer-name t))


(use-package company-mode
  :hook (after-init . global-company-mode))

(use-package counsel
  :config (ivy-mode 1))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (setq lsp-ui-doc-position 'bottom)
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
