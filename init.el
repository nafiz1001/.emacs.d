;; -*- lexical-binding: t; -*-

;; packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 10)
	("nongnu" . 9)
	("melpa" . 8)))

(package-initialize)

(package-install 'use-package)

(use-package no-littering
  :ensure t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

(use-package emacs
  :config
  ;; Borrowed a crap ton of configuration from https://github.com/daviwil/dotfiles/blob/master/Emacs.org
  
  ;; basic configs
  
  (setq inhibit-startup-message t)
  (setq inhibit-startup-screen t)
  (setq visible-bell t)

  (scroll-bar-mode 'right)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (menu-bar-mode t)
  (scroll-bar-mode -1)

  (setq echo-keystrokes 0.05)

  ;; Scrolling

  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

  (setq-default truncate-lines t) ;; avoid jumpy scrolling

  ;; Line/Column Number

  (column-number-mode)

  (global-display-line-numbers-mode nil)

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
  (setq native-comp-async-report-warnings-errors nil) ;; stop annoying warning popup

  ;; Coding System
  (set-default-coding-systems 'utf-8)

  ;; Recursive Minibuffer
  (setq enable-recursive-minibuffers  t)
  (minibuffer-depth-indicate-mode 1)

  ;; Electric Boogaloo
  ;; typing an open parenthesis automatically inserts the corresponding
  ;; closing parenthesis, and vice versa.  (Likewise for brackets, etc.).
  ;; If the region is active, the parentheses (brackets, etc.) are
  ;; inserted around the region instead
  (electric-pair-mode t)

  ;; font size adjustment
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

  ;; completions
  ;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
  (setq completions-format 'one-column)
  (setq suggest-key-bindings t)
  (setq completions-detailed t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  ;; utility functions

  (defun my/enable-remote-dir-locals ()
    "Allows loading .dir-locals.el that is in remote"
    (interactive)
    (setq enable-remote-dir-locals t))

  ;; use trash by default
  (setq delete-by-moving-to-trash t)

  ;; enable mouse
  (xterm-mouse-mode 1)

  ;; async-shell-command-buffer
  (setq async-shell-command-buffer 'confirm-rename-buffer)

  (dolist (mode '(prog-mode-hook
		  org-mode-hook))
    (add-hook mode #'display-fill-column-indicator-mode)))

(use-package hideif
  :hook ((c-mode c++-mode c-or-c++-mode) . hide-ifdef-mode))

(use-package windmove
  :config
  (global-set-key (kbd "C-x <left>")  'windmove-left)
  (global-set-key (kbd "C-x <right>") 'windmove-right)
  (global-set-key (kbd "C-x <up>")    'windmove-up)
  (global-set-key (kbd "C-x <down>")  'windmove-down))

(use-package paren
  :init
  (setq show-paren-style 'parenthesis)
  :config
  (show-paren-mode +1))

(use-package dired
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  (defun my/dired-find-file (filename &optional wildcards)
    (interactive
     (let ((default-directory (dired-current-directory)))
       (find-file-read-args "Find file: "
                            (confirm-nonexistent-file-or-buffer))))
    (find-file filename wildcards))
  (defun my/dired-hide-details-mode ()
    (dired-hide-details-mode +1))
  (define-key dired-mode-map "\C-x\C-f" #'my/dired-find-file)
  (setq wdired-allow-to-change-permissions t)
  (add-hook 'dired-mode-hook #'my/dired-hide-details-mode)
  (setq dired-listing-switches "-al"))

;; https://xenodium.com/emacs-viewing-webp-images/
(use-package image
  :custom
  (image-use-external-converter t)
  :init
  (add-hook 'image-mode-hook #'my/disable-line-number))

(use-package org
  :init
  (custom-set-variables
   '(org-directory "~/Documents/Org/")
   '(org-agenda-files (list org-directory)))
  (defun my/find-org-file (filename)
    (interactive
     (list (let* ((files (directory-files-recursively org-directory "\\(.org\\|.txt\\)$"))
		  (files (mapcar (lambda (f) (file-relative-name f org-directory)) files)))
	     (completing-read "Find org file: " files nil 'confirm))))
    (unless (string-match-p "\\(.org\\|.txt\\)$" filename)
      (setq filename (concat filename ".org")))
    (find-file (concat org-directory filename)))
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s") (todo . "%i")
				   (tags . " %i %-12:c") (search . " %i %-12:c"))))

(use-package doc-view
  :config
  (add-hook 'doc-view-mode-hook #'my/disable-line-number)
  (add-hook 'pdf-view-mode-hook #'my/disable-line-number))

(use-package eww
  :config
  (define-key eww-mode-map "W" #'shr-copy-url))

(use-package re-builder
  :init
  (setq reb-re-syntax 'read))

(use-package tab-bar
  :config
  (tab-bar-mode))

(use-package auth-source
  :config
  (setq auth-sources '("secrets:Login"
		       "secrets:session"
		       default)))

(use-package gnus
  :config
  (setq gnus-select-method '(nntp "news.gmane.io")))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi :no-confirm))

(use-package eglot
  :init
  (setq eldoc-idle-delay 0.75)
  (setq flymake-no-changes-timeout 0.5)
  :config
  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode)
		 .
		 ;; TODO: investigate why defun gives you error here but not lambda.
		 ,(lambda (&optional interactive)
		    `("jdtls" "-data" ,(concat temporary-file-directory "jdtls/cache/" (file-name-nondirectory (directory-file-name (vc-root-dir))))))))
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments))
  
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
		 ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(use-package debbugs
  :ensure t
  :commands (debbugs-gnu
	     debbugs-gnu-search
	     debbugs-gnu-usertags
	     debbugs-gnu-patches
	     debbugs-gnu-bugs))

(use-package breadcrumb
  :ensure t
  :commands (breadcrumb-mode breadcrumb-local-mode))

(use-package system-packages
  :disabled
  :ensure t)
(use-package use-package-ensure-system-package
  :disabled
  :after (system-packages)
  :ensure t)

(use-package which-key
  :disabled
  :ensure t
  :config
  (which-key-mode))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

(use-package obsidian
  :disabled
  :ensure t
  :config
  (obsidian-specify-path "~/Documents/Obsidian")
  (global-obsidian-mode t))

(use-package rainbow-delimiters 
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook ((org-mode prog-mode) . rainbow-mode))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (defun my/disable-hl-todo ()
    (hl-todo-mode -1))
  (add-hook 'org-mode-hook #'my/disable-hl-todo))

(use-package icomplete
  :config
  (icomplete-mode +1))

(use-package vertico
  :disabled
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :disabled
  :ensure t
  :bind (("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("C-S-f" . consult-grep)
	 ("C-S-p" . consult-find))
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :disabled
  :ensure t
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy
				marginalia-annotators-light
				nil))
  :config
  (marginalia-mode))

(use-package company
  :disabled
  :ensure t
  :init
  (setq company-idle-delay 0.5)
  :config
  (setq company-backends '((company-capf company-dabbrev-code)))
  (global-company-mode))

(use-package cape
  :disabled
  :ensure t
  :config
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)

  ;; Use Company backends as Capfs.
  (dolist (capf
	   (mapcar #'cape-company-to-capf
		   (list #'company-files #'company-keywords #'company-dabbrev)))
    (add-to-list 'completion-at-point-functions capf)))

(use-package magit
  :ensure t
  :commands (magit-status magit))

(use-package git-gutter
  :disabled
  :ensure t
  :config
  (global-git-gutter-mode))

(cond ((and (>= emacs-major-version 29) (eq system-type 'gnu/linux) (not (file-exists-p "/etc/NIXOS")))
       (use-package treesit
	 :config
	 (setq treesit-language-source-alist
	       '((java "https://github.com/tree-sitter/tree-sitter-java")
		 (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		 (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
		 (c "https://github.com/tree-sitter/tree-sitter-c")))

	 (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
	 (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
	 (add-to-list 'major-mode-remap-alist
                      '(c-or-c++-mode . c-or-c++-ts-mode))))
      (t
       (use-package tree-sitter
	 :ensure t
	 :hook (tree-sitter-after-on . tree-sitter-hl-mode)
	 :config
	 (global-tree-sitter-mode))
       (use-package tree-sitter-langs
	 :ensure t
	 :after (tree-sitter))))

(use-package ace-window
  :disabled
  :ensure t)

(use-package docker
  :ensure t
  :commands (docker docker-images docker-containers docker-volumes))

(use-package sly
  :disabled
  :ensure t
  :commands (sly)
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package tuareg
  :disabled
  :ensure t
  :commands (tuareg-mode))
(use-package merlin
  :disabled
  :ensure t
  :after (tuareg)
  :hook ((tuareg-mode caml-mode) . merlin-mode)
  :commands (merlin-mode))
(use-package merlin-company
  :disabled
  :ensure t
  :after (merlin company))

(defun my/rust-mode-hook ()
  (setq indent-tabs-mode nil))
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . my/rust-mode-hook)
  :commands (rust-mode))

;; backup lsp
(use-package lsp-mode
  :disabled
  :ensure t
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
  :disabled
  :ensure t
  :commands (lsp-ui-mode)
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-sideline-enable nil))
(use-package lsp-java
  :disabled
  :ensure t
  :after (lsp-mode))

;; eglot and obsidian needs this
(use-package markdown-mode
  :ensure t)

(use-package hyperbole
  :disabled
  :ensure t)

(unless (eq system-type 'windows-nt)
  (use-package vterm
    :disabled
    :ensure t
    :init
    (setq vterm-max-scrollback 1000)
    :config
    (add-hook 'vterm-mode-hook #'my/disable-line-number)))

(use-package nix-mode
  :ensure t
  :init
  (defun my/nixos-rebuild-switch-flake (hostname)
    "Run sudo nixos-rebuild switch --flake '.#HOSTNAME' in the current project's root directory."
    (interactive
     (progn
       (unless (json-available-p)
       (error "JSON library or support unavailable"))
       (let* ((default-directory (project-root (project-current t)))
	      (nix-flake-show (json-parse-string (shell-command-to-string "nix flake show --quiet --json 2>/dev/null")))
	      (keys (let (keys)
		      (maphash (lambda (k v) (push k keys)) nix-flake-show)
		      keys))
	      (type (completing-read "Pick type: " keys nil t))
	      (value (gethash type nix-flake-show))
	      (keys (let (keys)
		      (maphash (lambda (k v) (push k keys)) value)
		      keys))
	      (hostname (completing-read "Pick hostname: " keys nil t)))
	 (list hostname))))
    (let ((default-directory (project-root (project-current t))))
      (async-shell-command (format "sudo nixos-rebuild switch --flake '.#%s'" hostname)  "*nixos-rebuild*")))
  ;; TODO: still does not work when working with emacs source code
  (defun my/fix-nix-indentation ()
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2))
  (add-hook 'nix-mode-hook #'my/fix-nix-indentation))

(use-package zig-mode
  :disabled
  :ensure t
  :mode "\\.zig\\'"
  :commands (zig-mode))

(use-package apheleia
  :ensure t
  :config
  ;; temporarily set default-directory to root of project
  (defun shou/fix-apheleia-project-dir (orig-fn &rest args)
    (let ((default-directory (vc-root-dir)))
      (apply orig-fn args)))
  (advice-add 'apheleia-format-buffer :around #'shou/fix-apheleia-project-dir)
  (push '(spotless . ((concat default-directory "gradlew")
		      "spotlessApply"
		      (concat "-PspotlessIdeHook=" (apheleia-formatters-local-buffer-file-name))
		      "-PspotlessIdeHookUseStdIn"
		      "-PspotlessIdeHookUseStdOut"
   		      "--quiet"))
	apheleia-formatters))

(use-package flymake-eslint
  :disabled
  :ensure t
  :commands (flymake-eslint-enable)
  :hook (((js-mode typescript-ts-mode-hook tsx-ts-mode-hook)  . flymake-eslint-enable))
  :init
  (setq flymake-eslint-defer-binary-check t))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

(use-package atomic-chrome
  :ensure t
  :commands (atomic-chrome-start-server))
