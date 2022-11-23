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

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode #'my/disable-line-number))

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

;; LSP

(setq gc-cons-threshold (* 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; Paren

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; Coding System

(set-default-coding-systems 'utf-8)

;; Recursive Minibuffer

(setq enable-recursive-minibuffers  t)
(minibuffer-depth-indicate-mode 1)

;; Org

(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-startup-folded t)
(setq org-adapt-indentation nil)
(setq org-src-tab-acts-natively t)
(setq org-cycle-separator-lines 0)

(setq straight-vc-git-default-clone-depth 1)
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

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

(use-package setup
  :straight (setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))

(defmacro my/use-package-lazy (name &rest plist)
  ;; https://github.com/radian-software/straight.el/issues/235#issuecomment-366342968
  (let* ((disabled (plist-get plist :disabled))
	 (commands (plist-get plist :commands))
	 (commands (if (not (listp commands)) (list commands) commands))
	 (make-command (lambda (command)
			 `(defun ,command (&rest args)
    			    (interactive)
    			    (mapcar #'fmakunbound ',commands)
    			    (use-package ,name ,@plist)
    			    (apply ,command args))))
	 (command-body (if (and commands (not disabled))
			   `(progn ,@(mapcar make-command commands))
			 `(use-package ,name ,@plist)))
	 (deps (plist-get plist :after))
	 (eval-after-load-wrapper (lambda (acc elt) `(with-eval-after-load ',elt ,acc))))
    `(progn
       (if (straight-use-package-lazy ',name)
	   (use-package ,name ,@plist)
	 ,(seq-reduce eval-after-load-wrapper deps command-body)))))

(use-package no-littering
  :straight (no-littering :type git :host github :repo "emacscollective/no-littering")
  :init
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-region '(bg-only no-extend))
  :config
  (load-theme 'modus-vivendi t))

(use-package nix-mode
  :disabled
  :mode "\\.nix\\'")

(use-package rainbow-delimiters :hook prog-mode-hook)

(use-package hl-todo
  :bind (:map hl-todo-mode-map
	      ("C-c p" . hl-todo-previous)
	      ("C-c n" . hl-todo-next)
	      ("C-c o" . hl-todo-occur)
	      ("C-c i" . hl-todo-insert))
  :config
  (global-hl-todo-mode))

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package vterm
  :hook (vterm-mode . my/disable-line-number)
  :init
  (setq vterm-max-scrollback 1000)
  (defun my/new-term (buffer-name)
    (interactive "sbuffer name: ")
    (vterm vterm-shell)
    (rename-buffer buffer-name t)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function)
	 ("C-h C" . helpful-command)))

(use-package undo-tree
  :init
  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode 1))

(use-package evil
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
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after (evil)
  :init
  (setq evil-collection-company-use-tng nil)
  (setq evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after (evil)
  :config
  (evil-commentary-mode))

(use-package magit)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package company-mode
  :hook (after-init-hook . global-company-mode))

(use-package yasnippet
  :bind (:map yas-minor-mode-map
	      ("<tab>" . nil)
	      ("TAB" . nil)
	      ("C-c y" . yas-expand))
  :config
  (yas-global-mode 1))

(use-package vertico
  :config
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
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
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy
				   marginalia-annotators-light
				   nil))
  :config
  (marginalia-mode))

(use-package embark-consult)

(use-package embark
  :after (embark-consult)
  :bind (("C-S-a" . embark-act)
	 :map minibuffer-local-map
	 ("C-d" . embark-act)))

(use-package projectile
  :bind (("C-M-p" . projectile-find-file))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (projectile-mode))

(use-package flycheck
  :hook lsp-mode)

(use-package smartparens
  :hook prog-mode)

(use-package treemacs
  :disabled)
(use-package treemacs-evil
  :after (treemacs evil)
  :disabled)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :disabled)
(use-package treemacs-magit
  :after (treemacs magit)
  :disabled)
(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :disabled)

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :after (tree-sitter))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-signature-render-documentation nil))

(use-package lsp-ui
  :after (lsp-mode)
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-sideline-enable nil))

(add-to-list 'auto-mode-alist '("\\.[tj]sx?$" . javascript-mode))

(use-package json-mode :disabled)

(use-package auctex
  :disabled
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package rustic :disabled)

(use-package go-mode
  :disabled
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package slime
  :disabled
  :init
  (setq inferior-lisp-program (executable-find "sbcl" exec-path)))

(use-package cider
  :disabled
  :commands (cider-jack-in cider-connect cider-connect-cljs))

(use-package tuareg :disabled)
(use-package merlin :disabled)
(use-package merlin-company :disabled)
(use-package merlin-eldoc :disabled)

(use-package lsp-pyright
  :disabled
  :after (lsp-mode)
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]env\\'"))

(use-package envrc :disabled)
