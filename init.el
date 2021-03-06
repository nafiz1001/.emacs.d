;; -*- lexical-binding: t; -*-

;; https://stackoverflow.com/a/13983506
(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode 'right)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode t)

(defun my-configure-font ()
  "Configure font for first frame"
  (let ((my-font (lambda (n)
		   (concat
		    (face-attribute 'default :family)
		    "-"
		    (number-to-string n)))))
    (set-face-attribute 'default nil :font (funcall my-font 17))

    ;; https://emacs.stackexchange.com/a/1062
    (let ((faces '(mode-line
		   mode-line-buffer-id
		   mode-line-emphasis
		   mode-line-highlight
		   mode-line-inactive)))
      (mapc
       (lambda
	 (face)
	 (set-face-attribute face nil :font (funcall my-font 10)))
       faces)))
  (if (daemonp)
      (remove-hook 'server-after-make-frame-hook #'my-configure-font)
    (remove-hook 'window-setup-hook #'my-configure-font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my-configure-font)
  (add-hook 'window-setup-hook #'my-configure-font))

;; https://github.com/daviwil/dotfiles/blob/0e034ebb688633082a3b0c201b0bc834a9821091/Emacs.org
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(setq-default truncate-lines t) ;; avoid awkward scrolling

(column-number-mode)
(global-display-line-numbers-mode t)

;; prevent number lines in shell/term
;; https://github.com/daviwil/emacs-from-scratch/blob/d23348b4a52dde97f4f7cbcd66a519b5fd0a143c/init.el#L82-L88
(dolist (mode '(term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-auto-revert-mode 1)
(setq global-auto-revert-mode t)

(setq native-comp-async-report-warnings-errors nil)

(put 'dired-find-alternate-file 'disabled nil)

(setq gc-cons-threshold (* 1024 1024))
(setq read-process-output-max (* 1024 1024))

(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

(set-default-coding-systems 'utf-8)

(setq enable-recursive-minibuffers  t)
(minibuffer-depth-indicate-mode 1)

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)
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

(use-package no-littering
  :straight
  (no-littering
   :type git
   :host github
   :repo "emacscollective/no-littering")
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/sessions/") t)))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode)

(use-package hl-todo
  :straight
  (hl-todo
   :type git
   :host github
   :repo "tarsius/hl-todo")
  :config
  (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert)
  (global-hl-todo-mode))

(use-package nyan-mode
  :config (nyan-mode 1))

(use-package vterm
  :commands vterm
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  (setq vterm-max-scrollback 1000))

(defun new-term (buffer-name)
  (interactive "sbuffer name: ")
  (vterm vterm-shell)
  (rename-buffer buffer-name t))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package magit :commands magit-status)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package consult
  :bind (;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ("<help> a" . consult-apropos)            ;; orig. apropos-command
	 ;; M-g bindings (goto-map)
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  (autoload 'projectile-project-root "projectile"))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package company-mode
  :hook (after-init . global-company-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package projectile
  :config
  (projectile-mode +1))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t d"   . treemacs-select-directory)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package perspective
  :bind (("C-x b" . persp-switch-to-buffer*)
	 ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs :after tree-sitter)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-signature-render-documentation nil)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-sideline-enable nil))

(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-startup-folded t)
(setq org-adapt-indentation nil)
(setq org-src-tab-acts-natively t)
(setq org-cycle-separator-lines 0)

(use-package json-mode)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred))))

(use-package rustic)

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package cider)

(use-package paredit
  :commands enable-paredit-mode)

(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]env\\'")
