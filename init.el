;; -*- lexical-binding: t -*-

;;; Bootstrap

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

;;; Core

(use-package emacs
  :ensure nil
  :custom
  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 101)
  (delete-by-moving-to-trash t)
  (visible-bell t)
  :config
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil))

(use-package savehist
  :ensure nil
  :custom
  (savehist-mode t))

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t))

(use-package saveplace
  :ensure nil
  :custom
  (save-place-mode t))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

(use-package so-long
  :ensure nil
  :custom
  (global-so-long-mode t))

(use-package repeat
  :ensure nil
  :custom
  (repeat-mode t))

(use-package xt-mouse
  :ensure nil
  :custom
  (xterm-mouse-mode t))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(use-package artist
  :ensure nil
  :hook (artist-mode . (lambda () (setq indent-tabs-mode nil))))

;;; UI

(use-package emacs
  :ensure nil
  :custom
  (custom-enabled-themes '(modus-vivendi))
  (inhibit-startup-screen t))

(use-package simple
  :ensure nil
  :custom
  (column-number-mode t)
  (size-indication-mode t))

(use-package display-line-numbers
  :ensure nil
  :custom
  (global-display-line-numbers-mode t))

(use-package tool-bar
  :ensure nil
  :custom
  (tool-bar-mode nil))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-mode t))

(use-package which-key
  :ensure nil
  :custom
  (which-key-mode t))

(use-package mouse
  :ensure nil
  :custom
  (context-menu-mode t))

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

;;; Completion

(use-package minibuffer
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  (completion-styles '(basic partial-completion emacs22 substring flex))
  (completions-sort 'historical)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

(use-package icomplete
  :ensure nil
  :custom
  (fido-mode t))

(use-package completion-preview
  :ensure nil
  :custom
  (global-completion-preview-mode t))

;;; Files

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

;; dired font-lock
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

(use-package vlf
  :ensure t)

;;; Navigation

(use-package windmove
  :ensure nil
  :bind (("C-x <left>"  . windmove-left)
	     ("C-x <right>" . windmove-right)
	     ("C-x <up>"    . windmove-up)
	     ("C-x <down>"  . windmove-down)))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep)
  :custom-face
  (xref-file-header ((t (:weight bold :underline t)))))

;;; VC and projects

(use-package magit
  :ensure t)

;; magit-project-status is autoloaded, so this does not wait for Magit to load.
(use-package project
  :ensure nil
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

;;; Programming

;;;; Indentation

;; Precedence: .editorconfig and file variables, then dtrt-indent
;; detection, then the default offset of the major mode.
(use-package emacs
  :ensure nil
  :custom
  (indent-tabs-mode nil)
  (tab-width 4))

;; editorconfig.el loads later in this file, so declare the variable for
;; the byte compiler.
(defvar editorconfig-indent-size-vars)

(defun buffer-indentation-vars (size)
  "Return (VAR . SIZE) pairs that set the indent step of this major mode.
Use the same mapping as `.editorconfig' `indent_size'."
  (require 'editorconfig)
  ;; The mapping can return `eval' forms and variables of packages that
  ;; are not installed, for example `py-indent-offset' of python-mode.el.
  (seq-filter (lambda (pair) (and (symbolp (car pair))
                                  (not (eq (car pair) 'eval))
                                  (boundp (car pair))))
              (if (functionp editorconfig-indent-size-vars)
                  (funcall editorconfig-indent-size-vars size)
                (mapcar (lambda (var) (cons var size))
                        editorconfig-indent-size-vars))))

(defun buffer-indentation-size ()
  "Return the indent step of the current buffer."
  (if indent-tabs-mode
      tab-width
    (or (seq-some (lambda (pair)
                    (let ((value (symbol-value (car pair))))
                      (and (natnump value) value)))
                  (buffer-indentation-vars 1))
        standard-indent)))

(defun set-buffer-indentation (style size &optional convert)
  "Indent the current buffer with STYLE (\"Spaces\" or \"Tabs\") of SIZE.
With a prefix argument CONVERT, also convert the leading whitespace."
  (interactive
   (list (completing-read "Indent using: " '("Spaces" "Tabs") nil t nil nil
                          (if indent-tabs-mode "Tabs" "Spaces"))
         (read-number "Size: " (buffer-indentation-size))
         current-prefix-arg))
  (let ((tabs (equal style "Tabs"))
        (vars (buffer-indentation-vars size)))
    (setq-local indent-tabs-mode tabs
                tab-width size
                standard-indent size)
    (pcase-dolist (`(,var . ,value) vars)
      (set (make-local-variable var) value))
    (when convert
      ;; tabify.el is not loaded yet, so declare the variable dynamic here.
      (defvar tabify-regexp)
      ;; The default regexp also matches whitespace after code.
      (let ((tabify-regexp "^\t* [ \t]+"))
        (if tabs
            (tabify (point-min) (point-max))
          (untabify (point-min) (point-max)))))
    (force-mode-line-update)
    (message "%s: %d (%s)" style size
             (mapconcat (lambda (pair) (symbol-name (car pair))) vars ", "))))

(add-to-list 'mode-line-misc-info
             '(:eval
               (when (derived-mode-p 'prog-mode 'text-mode)
                 (propertize
                  (format "%s:%d " (if indent-tabs-mode "Tabs" "Spaces")
                          (buffer-indentation-size))
                  'mouse-face 'mode-line-highlight
                  'help-echo "mouse-1: Set indentation"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'set-buffer-indentation)))))

(use-package dtrt-indent
  :ensure t
  :custom
  (dtrt-indent-lighter nil)
  (dtrt-indent-verbosity 0)
  ;; bash-ts-mode sets up SMIE, so dtrt-indent skips it without this.
  (dtrt-indent-run-after-smie t)
  :config
  ;; Let .editorconfig and file variables win over detection.
  ;; dtrt-indent misses two cases:
  ;; 1. It compares variable names, so an obsolete alias such as
  ;;    `rust-ts-mode-indent-offset' does not match the real
  ;;    `rust-ts-indent-offset' that editorconfig sets.
  ;; 2. It records `dtrt-indent-explicit-tab-mode' but never reads it.
  ;;
  ;; After each dtrt-indent upgrade, test if this advice is still necessary:
  ;; 1. Make a directory with this .editorconfig:
  ;;      root = true
  ;;      [*]
  ;;      indent_style = tab
  ;;      indent_size = 8
  ;; 2. In that directory, make a .rs file with 2-space indentation and
  ;;    two or more blocks.
  ;; 3. Remove the advice for this session. Type M-: and evaluate:
  ;;      (advice-remove 'dtrt-indent-try-set-offset
  ;;                     'dtrt-indent-try-set-offset@respect-local-vars)
  ;; 4. If the .rs file is open, kill its buffer.
  ;; 5. Open the .rs file.
  ;; 6. Type C-h v rust-ts-indent-offset. If the value is 8, case 1 is fixed.
  ;; 7. Type C-h v indent-tabs-mode. If the value is t, case 2 is fixed.
  ;; 8. If case 1 is fixed, remove the `offset-set-by-local-vars' code.
  ;; 9. If case 2 is fixed, remove the `tabs' code.
  ;; 10. If both cases are fixed, remove this advice and this comment.
  (define-advice dtrt-indent-try-set-offset (:around (orig) respect-local-vars)
    (let* ((offset-var (nth 2 (dtrt-indent--search-hook-mapping major-mode)))
           (tabs indent-tabs-mode)
           (offset-set-by-local-vars
            (seq-some (lambda (pair)
                        (eq (indirect-variable (car pair))
                            (indirect-variable offset-var)))
                      file-local-variables-alist)))
      (when (and offset-var offset-set-by-local-vars)
        (setq dtrt-indent-explicit-offset t))
      (prog1 (funcall orig)
        (when dtrt-indent-explicit-tab-mode
          (setq-local indent-tabs-mode tabs)))))
  (dtrt-indent-global-mode 1))

;; Detection does not find the brace style. The default GNU style indents
;; a brace on its own line one extra level.
(use-package c-ts-mode
  :ensure nil
  :defer t
  :custom
  ;; In c-ts-mode, k&r and linux also indent a brace on its own line one
  ;; extra level. bsd does not, and it keeps K&R code unchanged.
  (c-ts-mode-indent-style 'bsd))

(use-package cc-mode
  :ensure nil
  :defer t
  :custom
  ;; In CC mode, "k&r" sets `c-basic-offset' to 5. "stroustrup" uses 4.
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "stroustrup"))))

;;;; Other

(use-package treesit
  :ensure nil
  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'ask))

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
  ;; :custom-face only sets the defface spec, and the modus theme spec wins over it.
  (custom-set-faces
   '(eglot-highlight-symbol-face ((t (:inherit bold :underline t))))))

(use-package editorconfig
  :ensure nil
  :config
  ;; The default maps json-ts-mode to js-indent-level, which it ignores.
  (add-to-list 'editorconfig-indentation-alist
               '(json-ts-mode json-ts-indent-offset))
  (editorconfig-mode 1))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode))

;; ctags
(use-package citre
  :ensure t
  :config
  (require 'citre-config))

(use-package hl-todo
  :ensure t
  :hook (prog-mode))

(use-package elec-pair
  :ensure nil
  :custom
  (electric-pair-mode t))

;; Eglot renders LSP documentation with gfm-view-mode from markdown-mode.
;; Without it, Eglot shows raw markdown in text-mode.
(use-package markdown-mode
  :ensure t)

;; markdown-ts-mode does not add itself to auto-mode-alist.
;; This block comes after markdown-mode, so its .md entry wins.
(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'")

(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-use-buttons 'in-margin)
  :hook (emacs-lisp-mode . outline-minor-mode))

;;; Remote and shells

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

(use-package proced
  :ensure nil
  :custom
  (proced-show-remote-processes t))

(use-package mistty
  :ensure t)

(use-package docker
  :ensure t)

;;; Org

(use-package org
  :ensure nil
  :custom
  (org-directory "~/Documents/Org")
  :config
  (add-hook 'org-mode-hook #'visual-line-mode))

;;; Commands

(defun unhighlight-all ()
  (interactive)
  (hi-lock-mode -1))

(defun copy-buffer-file-name ()
  (interactive)
  (kill-new (buffer-file-name)))

(defun copy-buffer-name ()
  (interactive)
  (kill-new (buffer-name)))

;;; Projects

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
