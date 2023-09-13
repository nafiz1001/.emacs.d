;; -*- lexical-binding: t; -*-

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

(setq echo-keystrokes 0.25)

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

;; https://stackoverflow.com/a/13983506
(defun my/nuke-other-buffers ()
  (interactive)
  (let* ((other-buffers (remove (current-buffer) (buffer-list))))
    (mapcar 'kill-buffer other-buffers)
    (delete-other-windows)))

(defun my/nuke-all-buffers ()
  (interactive)
  (when (fboundp 'eglot-shutdown-all)
    (eglot-shutdown-all))
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;; https://emacs.stackexchange.com/a/3172
(defun my/open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

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

;; straight.el

(setq straight-use-package-by-default nil)
(setq straight-vc-git-default-clone-depth 1)

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

;; packages

(use-package no-littering
  :straight t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

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
  :straight t
  :init
  (custom-set-variables
   '(org-directory "~/Documents/Org/")
   '(org-agenda-files (list org-directory)))
  (defun my/dired-org-file ()
    (interactive)
    (dired org-directory "-alR"))
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

(use-package modus-themes
  :straight t
  :config
  (load-theme 'modus-vivendi :no-confirm))

(use-package re-builder
  :init
  (setq reb-re-syntax 'read))

(use-package tab-bar
  :config
  (tab-bar-mode))

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
(use-package eglot-x
  :disabled
  :straight (eglot-x
	     :type git
	     :host github
	     :repo "nemethf/eglot-x")
  :after (eglot)
  :config
  (eglot-x-setup))

(use-package debbugs
  :disabled
  :straight t
  :commands (debbugs-gnu
	     debbugs-gnu-search
	     debbugs-gnu-usertags
	     debbugs-gnu-patches
	     debbugs-gnu-bugs))

(use-package system-packages
  :straight t)
(use-package use-package-ensure-system-package
  :disabled
  :after (system-packages)
  :ensure t)

(use-package which-key
  :disabled
  :straight t
  :config
  (which-key-mode))

(use-package xclip
  :straight t
  :config
  (xclip-mode 1))

(use-package vlf
  :straight t
  :config
  (require 'vlf-setup))

(use-package obsidian
  :disabled
  :straight t
  :config
  (obsidian-specify-path "~/Documents/Obsidian")
  (global-obsidian-mode t))

(use-package rainbow-delimiters 
  :straight t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (lisp-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :straight t
  :diminish
  :hook ((org-mode . rainbow-mode)
	 (prog-mode . rainbow-mode)))

(use-package hl-todo
  :straight t
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
  :straight t
  :config
  (vertico-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :disabled
  :straight t
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
  :straight t
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy
				marginalia-annotators-light
				nil))
  :config
  (marginalia-mode))

(use-package corfu
  :disabled
  :straight t
  :config
  (global-corfu-mode)
  (corfu-echo-mode)
  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu completion in the minibuffer, e.g., `eval-expression'."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer))
(use-package corfu-terminal
  :disabled
  :straight (corfu-terminal
	     :type git
	     :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package company
  :straight t
  :init
  (setq company-idle-delay 0.75)
  :config
  (setq company-backends '((company-capf company-dabbrev-code)))
  (global-company-mode))

(use-package cape
  :disabled
  :straight t
  :config
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
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
  :straight t
  :commands (magit-status magit))

(use-package git-gutter
  :disabled
  :straight t
  :config
  (global-git-gutter-mode))

(cond ((and (>= emacs-major-version 29) (eq system-type 'gnu/linux)))
      (use-package treesit
	:config
	;; (add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . tsx-ts-mode))
	;; (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
	(add-to-list 'auto-mode-alist '("\\.c[xp][xp]\\'" . c++-ts-mode))
	(setq treesit-language-source-alist
	      '((java "https://github.com/tree-sitter/tree-sitter-java")
		(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
		(cpp "https://github.com/tree-sitter/tree-sitter-cpp")))

	(treesit-install-language-grammar 'cpp)
	(setq major-mode-remap-alist
	      '((c++-mode . c++-ts-mode)
		(c-or-c++-mode . c++-ts-mode))))
      (t
       (use-package tree-sitter
	 :straight t
	 :hook (tree-sitter-after-on . tree-sitter-hl-mode)
	 :config
	 (global-tree-sitter-mode))
       (use-package tree-sitter-langs
	 :straight t
	 :after (tree-sitter))))

(use-package ace-window
  :disabled
  :straight t)

(use-package docker
  :straight t
  :commands (docker))

(use-package sly
  :disabled
  :straight t
  :commands (sly)
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package tuareg
  :disabled
  :straight t
  :commands (tuareg-mode))
(use-package merlin
  :disabled
  :straight t
  :after (tuareg)
  :hook ((tuareg-mode . merlin-mode)
	 (caml-mode . merlin-mode))
  :commands (merlin-mode))
(use-package merlin-company
  :disabled
  :straight t
  :after (merlin company))

(defun my/rust-mode-hook ()
  (setq indent-tabs-mode nil))
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :hook (rust-mode . my/rust-mode-hook)
  :commands (rust-mode))

;; backup lsp
(use-package lsp-mode
  :disabled
  :straight t
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
  :straight t
  :commands (lsp-ui-mode)
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-sideline-enable nil))
(use-package lsp-java
  :disabled
  :straight t
  :after (lsp-mode))

;; eglot and obsidian needs this
(use-package markdown-mode
  :straight t)

(use-package hyperbole
  :straight t)

(unless (eq system-type 'windows-nt)
  (use-package vterm
    :disabled
    :straight t
    :init
    (setq vterm-max-scrollback 1000)
    :config
    (add-hook 'vterm-mode-hook #'my/disable-line-number)))

(use-package nix-mode
  :disabled
  :straight t
  :mode "\\.nix\\'")

(use-package zig-mode
  :disabled
  :straight t
  :mode "\\.zig\\'"
  :commands (zig-mode))

(use-package apheleia
  :straight t
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
  :straight t
  :commands (flymake-eslint-enable)
  :hook (((js-mode typescript-ts-mode-hook tsx-ts-mode-hook)  . flymake-eslint-enable))
  :init
  (setq flymake-eslint-defer-binary-check t))

(use-package envrc
  :disabled
  :straight t
  :config
  (envrc-global-mode))

(use-package atomic-chrome
  :straight t
  :commands (atomic-chrome-start-server))
