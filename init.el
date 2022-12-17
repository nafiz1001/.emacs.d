;; -*- lexical-binding: t; -*-

;; Borrowed a crap ton of configuration from https://github.com/daviwil/dotfiles/blob/master/Emacs.org

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; https://stackoverflow.com/a/13983506
(defun my/nuke-other-buffers ()
  (interactive)
  (let* ((other-buffers (remove (current-buffer) (buffer-list))))
    (mapcar 'kill-buffer other-buffers)
    (delete-other-windows)))

;; UI

(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq visible-bell t)

(scroll-bar-mode 'right)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode t)
(scroll-bar-mode -1)

;; Font

(defun my/configure-font ()
  "Configure font for first frame"
  (let ((my-font (lambda (n)
		   (concat
		    (face-attribute 'default :family)
		    "-"
		    (number-to-string n))))
	;; https://emacs.stackexchange.com/a/1062
	(faces '(mode-line
		  mode-line-buffer-id
		  mode-line-emphasis
		  mode-line-highlight
		  mode-line-inactive)))
    (set-face-attribute 'default nil :font (funcall my-font 15))
    (mapc
     (lambda (face) (set-face-attribute face nil :font (funcall my-font 10)))
     faces))
  (if (daemonp)
      (remove-hook 'server-after-make-frame-hook #'my/configure-font)
    (remove-hook 'window-setup-hook #'my/configure-font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/configure-font)
  (add-hook 'window-setup-hook #'my/configure-font))

;; Scrolling

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(setq-default truncate-lines t) ;; avoid awkward scrolling

;; Line/Number

(column-number-mode)

(global-display-line-numbers-mode t)

(defun my/disable-line-number ()
  (display-line-numbers-mode 0))

(defun my/enable-line-number ()
  (display-line-numbers-mode 1))

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode #'my/enable-line-number))

;; prevent number lines in shell/term
;; https://github.com/daviwil/emacs-from-scratch/blob/d23348b4a52dde97f4f7cbcd66a519b5fd0a143c/init.el#L82-L88
(dolist (mode '(term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode #'my/disable-line-number))

;; Auto-revert

(global-auto-revert-mode 1)
(setq global-auto-revert-mode t)

;; Native Compilation

(setq native-comp-async-report-warnings-errors nil)

;; Dired

(put 'dired-find-alternate-file 'disabled nil)

;; Paren

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; Coding System

(set-default-coding-systems 'utf-8)

;; Recursive Minibuffer

(setq enable-recursive-minibuffers  t)
(minibuffer-depth-indicate-mode 1)

;; Straight

(setq straight-vc-git-default-clone-depth '(1 single-branch))
(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(setq use-package-always-defer t)
(setq use-package-always-demand nil)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

(use-package setup
  :straight (setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))

(defun use-package-normalize/:lazy (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      arg)
    t))

(defun use-package-handler/:lazy (name keyword arg rest state)
  (let* ((commands (plist-get rest :commands))
	 (deps (plist-get rest :after))
	 (body (use-package-process-keywords name rest state)) 
	 (bootstrappers (cond
			 ((equal arg 't)
			  `(funcall exec-body))
			 (arg
			  (let ((name arg))
			    `(defun ,name ()
			       ,(format "Install %s" name)
    			       (interactive)
			       (when (y-or-n-p ,(format "Do you wish to clone and use %s?" name))
				 (progn (mapcar #'fmakunbound '(,name))
    					(funcall exec-body))))))
			 (commands
			  (let ((make-command (lambda (command)
						`(defun ,command (&rest args)
						   ,(format "Install %s and then execute the true %s" name command)
    						   (interactive)
						   (when (y-or-n-p ,(format "Do you wish to clone and use %s?" name))
						     (progn (mapcar #'fmakunbound ',commands)
    							    (funcall exec-body)
							    (if (called-interactively-p 'any)
								(call-interactively #',command)
							      (apply #',command args))))))))
			    `(progn ,@(mapcar make-command commands))))
			 (t
			  (use-package-error "invalid combination of :lazy and :commands"))))
	 (eval-after-load-wrapper (lambda (acc elt) `(with-eval-after-load ',elt ,acc))))
    `((let ((exec-body (lambda () ,@body)))
	(if (straight-use-package-lazy ',name)
	    (funcall exec-body)
	  ,(seq-reduce eval-after-load-wrapper deps bootstrappers))))))

(add-to-list 'use-package-keywords :lazy)

(use-package tramp
  :lazy tramp
  :init
  (defun tramp ()
    (require 'tramp)))

(use-package org
  :lazy
  :commands (org-mode)
  :config
  (setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
  (setq org-startup-folded t)
  (setq org-adapt-indentation nil)
  (setq org-src-tab-acts-natively t)
  (setq org-cycle-separator-lines 0))

(use-package no-littering
  :demand t
  :straight (no-littering :type git :host github :repo "emacscollective/no-littering")
  :init
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

(use-package modus-themes
  :demand t
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-region '(bg-only no-extend))
  :config
  (load-theme 'modus-vivendi t))

(use-package rainbow-delimiters
  :demand t
  :hook prog-mode)

(use-package hl-todo
  :lazy
  :hook (prog-mode . hl-todo-mode)
  :commands (hl-todo-mode)
  :bind (:map hl-todo-mode-map
	      ("C-c p" . hl-todo-previous)
	      ("C-c n" . hl-todo-next)
	      ("C-c o" . hl-todo-occur)
	      ("C-c i" . hl-todo-insert))
  :config
  (global-hl-todo-mode))

(use-package nyan-mode
  :demand t
  :config
  (nyan-mode 1))

(use-package vterm
  :lazy
  :commands (vterm my/new-term)
  :hook (vterm-mode . my/disable-line-number)
  :init
  (setq vterm-max-scrollback 1000)
  (defun my/new-term (buffer-name)
    (interactive "sbuffer name: ")
    (vterm vterm-shell)
    (rename-buffer buffer-name t)))

(use-package helpful
  :demand t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function)
	 ("C-h C" . helpful-command)))

(use-package undo-tree
  :demand t
  :init
  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode 1))

(use-package evil
  :demand t
  :after (undo-tree)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)

  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode
		  artist-mode))
    (progn (add-to-list 'evil-emacs-state-modes mode)
	   (evil-set-initial-state mode 'emacs)))

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :demand t
  :after (evil)
  :init
  (setq evil-collection-company-use-tng nil)
  (setq evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :demand t
  :after (evil)
  :config
  (evil-commentary-mode))

(use-package magit
  :lazy
  :commands (magit))

(use-package git-gutter
  :demand t
  :config
  (global-git-gutter-mode +1))

(use-package company-mode
  :hook (after-init-hook . global-company-mode))

(use-package yasnippet
  :lazy
  :commands (yas-expand)
  :bind (:map yas-minor-mode-map
	      ("<tab>" . nil)
	      ("TAB" . nil)
	      ("C-c y" . yas-expand))
  :config
  (yas-global-mode 1))

(use-package vertico
  :demand t
  :config
  (vertico-mode))

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
	 ("C-M-l" . consult-imenu)
	 ("C-M-j" . persp-switch-to-buffer*)
	 :map minibuffer-local-map
	 ("C-r" . consult-history))
  :init
  (defun dw/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))
  (setq consult-project-root-function #'dw/get-project-root
	   completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :demand t
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy
				   marginalia-annotators-light
				   nil))
  :config
  (marginalia-mode))

(use-package embark
  :lazy
  :commands (embark-act embark-act-all embark-collect embark-export)
  :bind (("C-S-a" . embark-act)
	 :map minibuffer-local-map
	 ("C-d" . embark-act)))
(use-package embark-consult
  :lazy t
  :commands (embark-collect embark-export)
  :after (embark consult))

(use-package projectile
  :lazy
  :commands (projectile-mode projectile-find-file)
  :bind (("C-M-p" . projectile-find-file))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (projectile-mode))

(use-package flycheck
  :commands (flycheck-mode global-flycheck-mode)
  :hook lsp-mode)

(use-package smartparens
  :hook prog-mode
  :config
  (require 'smartparens-config))

(use-package treemacs
  :lazy
  :commands (treemacs))
(use-package treemacs-evil
  :lazy t
  :commands (treemacs)
  :after (treemacs evil))
(use-package treemacs-projectile
  :lazy t
  :commands (treemacs)
  :after (treemacs projectile))
(use-package treemacs-magit
  :lazy t
  :commands (treemacs)
  :after (treemacs magit))
(use-package lsp-treemacs
  :lazy t
  :commands (treemacs)
  :after (treemacs lsp-mode))

(use-package tree-sitter
  :lazy
  :demand t
  :commands (global-tree-sitter-mode)
  :hook ((tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :lazy t
  :demand t
  :after (tree-sitter))

(use-package lsp-mode
  :lazy
  :commands (lsp lsp-deferred)
  :init
  (setq gc-cons-threshold (* 1024 1024))
  (setq read-process-output-max (* 1024 1024))

  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-keep-workspace-alive nil)

  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-signature-render-documentation nil))

(use-package lsp-ui
  :lazy t
  :commands (lsp-ui-mode)
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-sideline-enable nil))

(use-package dap-mode
  :lazy
  :after (lsp-mode)
  :commands (dap-mode))

(add-to-list 'auto-mode-alist '("\\.[tj]sx?$" . javascript-mode))

(use-package pyvenv
  :lazy
  :commands (pyvenv-activate pyvenv-workon))

(use-package nix-mode
  :lazy
  :commands (nix-mode))

(use-package json-mode
  :lazy
  :commands (json-mode))

(use-package auctex
  :lazy auctex
  :hook (latex-mode TeX-mode plain-tex-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package rustic
  :lazy
  :commands (rustic-mode))

(use-package go-mode
  :lazy
  :commands (go-mode))

(use-package slime
  :lazy
  :commands (slime)
  :init
  (setq inferior-lisp-program (executable-find "sbcl" exec-path)))

(use-package cider
  :lazy
  :commands (cider cider-jack-in cider-jack-in-cljs cider-connect cider-connect-cljs))

(use-package tuareg
  :lazy
  :commands (tuareg-mode run-ocaml ocamldebug))
(use-package merlin
  :lazy
  :commands (merlin-mode))
(use-package merlin-company
  :lazy t
  :after (merlin company))
(use-package merlin-eldoc
  :lazy t
  :after (merlin))

(use-package lsp-pyright
  :lazy lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)
			 (require 'dap-python)))
  :init
  (setq dap-python-debugger "debugpy")
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/src/github.com/microsoft/python-type-stubs"))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]v?env\\'"))

(use-package envrc
  :lazy
  :commands (envrc-global-mode))

(use-package ein
  :lazy
  :commands (ein:run ein:login))

(use-package scala-mode
  :lazy
  :commands (scala-mode)
  :interpreter ("scala" . scala-mode))
(use-package sbt-mode
  :lazy
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
(use-package lsp-metals
  :lazy lsp-metals
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . lsp-deferred))
