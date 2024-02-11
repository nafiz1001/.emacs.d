;; -*- lexical-binding: t; -*-

;; Based on https://git.sr.ht/~ashton314/emacs-bedrock/refs/1.3.1

;;; Guardrail

(when (< emacs-major-version 29)
  (error (format "Emacs Bedrock only works with Emacs 29 and newer; you have version ~a" emacs-major-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Base settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Don't litter files
(use-package no-littering
  :ensure t
  :config
  (setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

;; If you want to turn off the welcome screen, uncomment this
(setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Move through windows
(use-package windmove
  :config
  (keymap-global-set "C-x <left>"  'windmove-left)
  (keymap-global-set "C-x <right>" 'windmove-right)
  (keymap-global-set "C-x <up>"    'windmove-up)
  (keymap-global-set "C-x <down>"  'windmove-down))

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; typing an open parenthesis automatically inserts the corresponding
;; closing parenthesis, and vice versa.  (Likewise for brackets, etc.).
(electric-pair-mode t)
(electric-indent-mode -1)
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
    (add-hook hook #'electric-indent-local-mode))

;; use trash by default
(setopt delete-by-moving-to-trash t)

;; enable mouse
(xterm-mouse-mode 1)

;; async-shell-command-buffer
(setopt async-shell-command-buffer 'confirm-rename-buffer)

;; highlight parenthesis
(use-package paren
  :config
  (setopt show-paren-style 'parenthesis)
  (show-paren-mode +1))

;; dired
(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (defun my/dired-find-file (filename &optional wildcards)
    (interactive
     (let ((default-directory (dired-current-directory)))
       (find-file-read-args "Find file: "
                            (confirm-nonexistent-file-or-buffer))))
    (find-file filename wildcards))
  (defun my/dired-hide-details-mode ()
    (dired-hide-details-mode +1))
  (keymap-set dired-mode-map "C-x C-f" #'my/dired-find-file)
  (setopt wdired-allow-to-change-permissions t)
  (add-hook 'dired-mode-hook #'my/dired-hide-details-mode))

;; https://xenodium.com/emacs-viewing-webp-images/
(use-package image
  :config
  (setopt image-use-external-converter t))

;; regex builder
(use-package re-builder
  :config
  (setopt reb-re-syntax 'read))

;; tramp remote .dir-locals.el
(setopt enable-remote-dir-locals t)
(use-package tramp
  :config
  (setopt tramp-methods (seq-filter (lambda (m) (not (equal (car m) "nixdev"))) tramp-methods))
  (push '("nixdev"
	  (tramp-login-program "nix")
	  (tramp-login-args
	   (("develop")))
	  (tramp-remote-shell "/bin/sh"))
	tramp-methods))

;; hide-ifdef-mode
(use-package hideif
  :config
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook #'hide-ifdef-mode)))

(use-package xref
  :if (executable-find "rg")
  :config
  (setopt xref-search-program 'ripgrep))

;; (defun my/vc-git-grep ()
;;   (interactive)
;;   (let* ((regexp (project--read-regexp))
;; 	 (pr (project-current t))
;; 	 (default-directory (project-root pr))
;; 	 (dir (read-directory-name "In directory: "
;; 				   nil default-directory t))
;; 	 (current-prefix-arg nil))
;;     (vc-git-grep regexp (cdr (assoc "all" grep-files-aliases)) dir)))
;; (advice-add #'project-find-regexp :override #'my/vc-git-grep)

(defun my/remove-tags-completion-at-point-function ()
  (setopt completion-at-point-functions (remove 'tags-completion-at-point-function completion-at-point-functions)))
(add-hook 'prog-mode-hook 'my/remove-tags-completion-at-point-function)
(add-hook 'text-mode-hook 'my/remove-tags-completion-at-point-function)

(defun my/dumb-indent ()
  "Insert a newline."
  (interactive "*")
  (electric-indent-just-newline nil)
  (indent-relative))

(global-set-key "\C-j" #'my/dumb-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 't)                   ; When I hit TAB, try to complete, otherwise, indent
;; (setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'lazy)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" #'minibuffer-complete) ; TAB acts more like how it does in the shell

(setopt suggest-key-bindings t)
(setopt read-buffer-completion-ignore-case t)
(setopt read-file-name-completion-ignore-case t)

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

(icomplete-mode +1)
;; (icomplete-vertical-mode)
;; (fido-mode)
;; (fido-vertical-mode)
;; (setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; don't show trailing white space
;; (setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; precise scrolling
(setopt mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setopt mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setopt scroll-step 1) ;; keyboard scroll one line at a time
(setopt scroll-conservatively 100)

;; other scrolling options
(setopt mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
;; (pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(dolist (mode '(org-mode-hook))
  (add-hook mode #'visual-line-mode))

;; vertical line bar indicating max width
(dolist (mode '(prog-mode-hook org-mode-hook))
  (add-hook mode #'display-fill-column-indicator-mode))

;; Original UI configs

(setopt inhibit-startup-message t)
(setopt inhibit-startup-screen t)
(setopt visible-bell t)

(scroll-bar-mode 'right)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode t)
(scroll-bar-mode -1)

(setopt echo-keystrokes 0.05)

(setopt use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(setopt truncate-lines t) ;; avoid jumpy scrolling

;; font size adjustment
(keymap-global-set "C-=" 'text-scale-increase)
(keymap-global-set "C--" 'text-scale-decrease)
(keymap-global-set "C-<wheel-up>" 'text-scale-increase)
(keymap-global-set "C-<wheel-down>" 'text-scale-decrease)

(add-to-list 'auto-mode-alist '("Kbuild" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.lds\\.h\\'" . asm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar
(setopt tab-bar-show t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  (load-theme 'modus-vivendi :no-confirm))          ; for light theme, use modus-operandi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Org
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :config
  (custom-set-variables '(org-directory "~/Documents/Org/")
			'(org-agenda-files (list org-directory)))
  (defun my/find-org-file (filename)
    (interactive
     (list (let* ((files (directory-files-recursively org-directory "\\(.org\\|.txt\\)$"))
		  (files (mapcar (lambda (f) (file-relative-name f org-directory)) files)))
	     (completing-read "Find org file: " files nil 'confirm))))
    (unless (string-match-p "\\(.org\\|.txt\\)$" filename)
      (setq filename (concat filename ".org")))
    (find-file (concat org-directory filename)))
  (defun my/diary-capture-advice (oldfun r)
    (let ((org-capture-templates `(("d"
				    "Diary Template"
				    item
				    (file+headline (lambda () (expand-file-name
							       (concat org-directory "Diary" "/" (format-time-string "%Y-%m-%d.org"))))
						   ,(format-time-string (org-time-stamp-format t t) (current-time)))
				    "- %?"
				    :empty-lines 0))))
      (call-interactively oldfun)))
  (advice-add #'org-capture :around #'my/diary-capture-advice))

(use-package org
  :after (treesit)
  :config
  (setopt org-src-lang-modes '(("c" . c-ts)
			       ("c++" . c++-ts)
			       ("asymptote" . asy)
			       ("bash" . bash-ts)
			       ("beamer" . latex)
			       ("calc" . fundamental)
			       ("cpp" . c++-ts)
			       ("ditaa" . artist)
			       ("desktop" . conf-desktop)
			       ("dot" . fundamental)
			       ("elisp" . emacs-lisp)
			       ("ocaml" . tuareg)
			       ("python" . python-ts-mode)
			       ("screen" . shell-script)
			       ("shell" . sh)
			       ("sqlite" . sql)
			       ("toml" . conf-toml))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   LSP
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :config
  (setopt eldoc-idle-delay 0.75)
  (setopt flymake-no-changes-timeout 0.5)
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (setopt eglot-send-changes-idle-time 0.1)

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
		 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs `(((js-mode :language-id "javascript")
					 (typescript-mode  :language-id "typescript"))
					.
					("typescript-language-server" "--stdio"))))

(use-package eglot
  :after (treesit)
  :config
  (add-to-list 'eglot-server-programs `(((js-mode :language-id "javascript")
                                         (js-ts-mode :language-id "javascript")
					 (tsx-ts-mode  :language-id "typescriptreact")
					 (typescript-ts-mode :language-id "typescript")
					 (typescript-mode  :language-id "typescript"))
					.
					("typescript-language-server" "--stdio"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tree-sitter
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (treesit-available-p)
  (use-package treesit
    :config
    (setopt treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.20.5")
        (c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.8")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.5")
	(go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2")
        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.1")
        (java "https://github.com/tree-sitter/tree-sitter-java" "v0.20.2")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.3")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
        (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.20.4")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")))

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))))

(use-package treesit-auto
  :after (treesit)
  :ensure t
  :config
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Email
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; authentication sources
(use-package auth-source
  :config
  (setopt auth-sources '("secrets:Login"
			 "secrets:session"
			 default)))

;; gnus: mailing-list, newsletter
(use-package gnus
  :config
  (setopt gnus-select-method '(nntp "news.gmane.io")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Extensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package orderless
  :ensure t
  :config
  (setopt completion-styles '(orderless)))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

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

(use-package magit
  :ensure t
  :demand t ;; maybe show magit option for `project-switch-project'
  :commands (magit-status magit)
  :bind (("C-x g" . magit-status)))

(use-package evil
  :disabled
  :ensure t

  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)

  ;; Enable this if you want C-u to scroll up, more like pure Vim
  ;(setq evil-want-C-u-scroll t)

  :hook ((prog-mode text-mode fundamental-mode) . evil-local-mode)

  :config
  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package apheleia
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

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

(use-package atomic-chrome
  :commands (atomic-chrome-start-server))

(use-package wgrep
  :ensure t
  :config
  (setopt wgrep-auto-save-buffer nil))

(use-package citre
  :disabled
  :config
  (require 'citre-config))

;; eglot's eldoc and obsidian needs this
(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode))

(use-package nix-mode
  :config
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
  ;; FIXME: still does not work when .dir-locals.el conflicts
  (defun my/fix-nix-indentation ()
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2)
    (electric-indent-local-mode))
  (add-hook 'nix-mode-hook #'my/fix-nix-indentation))

(defun my/rust-mode-hook ()
  (setq-local indent-tabs-mode nil))
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . my/rust-mode-hook)
  :commands (rust-mode))

(use-package gdscript-mode)

(use-package json-mode
  :ensure t)

(use-package flymake-eslint
  :if (executable-find "eslint")
  :config
  (flymake-eslint-enable))
