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
  (interactive)
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
    (set-face-attribute 'default nil :font (funcall my-font 16))
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

(setq-default truncate-lines t) ;; avoid jumpy scrolling

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

(setq native-comp-async-report-warnings-errors nil) ;; stop annoying warning popup

;; Dired

(put 'dired-find-alternate-file 'disabled nil)

;; Coding System

(set-default-coding-systems 'utf-8)

;; Recursive Minibuffer

(setq enable-recursive-minibuffers  t)
(minibuffer-depth-indicate-mode 1)

;; Paren

(setq show-paren-style 'parenthesis)
(require 'paren)
(show-paren-mode +1)

;; Electric Boogaloo

(electric-pair-mode t)

;; enable remove dir-locals
(defun my/enable-remote-dir-locals ()
  (interactive)
  (setq enable-remote-dir-locals t))

;; font size adjustment

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; straight.el

(setq straight-use-package-by-default nil)
(setq straight-vc-git-default-protocol 'ssh)
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
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory))))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

(use-package org
  :init
  (setq org-agenda-files "~/Documents/Org/")
  (setq org-startup-folded t)
  (setq org-adapt-indentation nil)
  (setq org-src-tab-acts-natively t)
  (setq org-cycle-separator-lines 0))

(use-package rainbow-delimiters 
  :straight t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (lisp-mode . rainbow-delimiters-mode)))

(use-package modus-themes
  :straight (modus-themes
	     :host github
	     :repo "protesilaos/modus-themes")
  :config
  (load-theme 'modus-vivendi :no-confirm))
(use-package catppuccin-theme
  :disabled
  :straight t
  :init
  (setq catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm))
(use-package dracula-theme
  :disabled
  :straight (dracula-theme
	     :type git
	     :host github
	     :repo "dracula/emacs"
	     :local-repo "dracula-theme")
  :config
  (load-theme 'dracula :no-confirm))

(use-package rainbow-mode
  :straight t
  :diminish
  :hook org-mode prog-mode)

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package vertico
  :straight t
  :config
  (vertico-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :straight t)

(use-package marginalia
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
  :straight (corfu-terminal
	     :type git
	     :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package cape
  :after (corfu)
  :straight t
  :init
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :config
  (bind-key "C-M-i" #'company-complete)
  (setq company-idle-delay nil))

(use-package magit
  :straight t
  :commands (magit-status))

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode +1))

(cond ((and (>= emacs-major-version 29) (not (eq system-type 'darwin)))
       (use-package treesit
	 :config
	 (add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . tsx-ts-mode))
	 (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))))
      (t
       (use-package tree-sitter
	 :straight t
	 :hook (tree-sitter-after-on . tree-sitter-hl-mode)
	 :config
	 (global-tree-sitter-mode))
       (use-package tree-sitter-langs
	 :straight t
	 :after (tree-sitter))))

(use-package eglot
  :ensure t
  :commands (eglot)
  :config
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdt-language-server" "-data" "/tmp/jdtls-cache")))
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments))

  (add-to-list 'eglot-server-programs
              '(nix-mode . ("nixd"))))
(use-package eglot-x
  :disabled
  :straight (eglot-x
	     :type git
	     :host github
	     :repo "nemethf/eglot-x")
  :after (eglot)
  :config
  (eglot-x-setup))

;; backup lsp
(use-package lsp-mode
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
  :straight t
  :commands (lsp-ui-mode)
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-sideline-enable nil))
(use-package lsp-java
  :straight t
  :after (lsp-mode))

(use-package markdown-mode
  :straight t)

(unless (eq system-type 'windows-nt)
  (use-package vterm
    ; install using package manager
    :init
    (setq vterm-max-scrollback 1000)
    :config
    (defun my/new-term (buffer-name)
      (interactive "sbuffer name: ")
      (vterm vterm-shell)
      (rename-buffer buffer-name t))
    (add-hook 'vterm-mode-hook #'my/disable-line-number)))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1))

(use-package flymake-eslint
  :straight t
  :commands (flymake-eslint-enable)
  :hook (((js-mode typescript-ts-mode-hook tsx-ts-mode-hook)  . flymake-eslint-enable))
  :init
  (setq flymake-eslint-defer-binary-check t))

(use-package envrc
  :straight t
  :config
  (envrc-global-mode))
