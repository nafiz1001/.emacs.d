;; -*- lexical-binding: t -*-

;;; Bootstrap

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			               ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			               ("melpa" . "https://melpa.org/packages/")))

;; Must be set before use-package loads. Adds a "Packages" group to imenu.
(setopt use-package-enable-imenu-support t)

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
  ;; `dashboard-setup-startup-hook' skips `emacs --daemon', so client
  ;; frames need this to show the dashboard.
  (initial-buffer-choice (lambda ()
                           (dashboard-open)
                           (get-buffer dashboard-buffer-name)))
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

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

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
  (define-key project-prefix-map "m" #'magit-project-status)
  ;; project-buffers expands the default-directory of every buffer.  For
  ;; a stale TRAMP buffer, such as one for "/podman::", TRAMP opens a
  ;; new connection, which fails if the host does not exist.  With
  ;; non-essential, TRAMP uses only connections that are already open.
  (cl-defmethod project-buffers :around (_project)
    (let ((non-essential t))
      (cl-call-next-method))))

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

;; Precedence: .editorconfig and file variables, then tree-sitter
;; detection, then the default offset of the major mode.
(use-package emacs
  :ensure nil
  :custom
  (indent-tabs-mode nil)
  (tab-width 4))

(use-package ts-indent-guess
  :ensure nil
  :load-path "lisp/ts-indent-guess"
  :config
  (ts-indent-guess-mode 1))

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

(use-package python
  :ensure nil
  ;; python-ts-mode enables node-based `sexp' navigation. Then
  ;; `backward-sexp' after a closing paren that ends a method moves to
  ;; the `def', not to the matching paren. Use list-based navigation,
  ;; which is the treesit default. The hook sets the same values as
  ;; `treesit-cycle-sexp-thing' gives in list mode. It does not call
  ;; that command, because the command toggles, so a later change of
  ;; the python-ts-mode default would flip the result.
  :hook (python-ts-mode . (lambda ()
                            (setq-local treesit-sexp-thing nil
                                        forward-sexp-function
                                        #'treesit-forward-sexp-list))))

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
   '(eglot-highlight-symbol-face ((t (:inherit bold :underline t)))))
  ;; Eglot's imenu index drops the position of a symbol that has
  ;; children, so imenu cannot jump to a class or function signature.
  ;; The tree-sitter index keeps it. Cost: imenu loses LSP-only
  ;; symbols, such as local variables, and symbol kinds from the server.
  ;; In modes without tree-sitter, imenu uses the major mode's own index.
  (add-to-list 'eglot-stay-out-of 'imenu))

;; DAP client in the style of Eglot. The key prefix is C-x C-a, as in gud.
(use-package dape
  :ensure t
  :custom
  ;; Source on the left and info buffers on the right, as in `gdb-many-windows'.
  (dape-buffer-window-arrangement 'gud)
  :config
  ;; Toggle breakpoints with a mouse click in the left fringe or margin.
  (dape-breakpoint-global-mode 1))

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
  :ensure t
  :config
  ;; Bug in markdown-mode: if the source buffer of a live preview dies
  ;; first, the preview keeps a pointer to the dead buffer.  The kill
  ;; hook of the preview then calls with-current-buffer on it, which
  ;; signals "Selecting deleted buffer".  An error in kill-buffer-hook
  ;; aborts kill-buffer, so the preview (an eww buffer) cannot be
  ;; killed.  Clear the dead pointer first.
  (advice-add 'markdown-live-preview-remove-on-kill :before
              (lambda ()
                (unless (buffer-live-p markdown-live-preview-source-buffer)
                  (setq markdown-live-preview-source-buffer nil)))))

;; markdown-ts-mode does not add itself to auto-mode-alist.
;; This block comes after markdown-mode, so its .md entry wins.
(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'"
  ;; Line numbers make each redisplay in this mode allocate about 40%
  ;; more. A read-only view does not need them.
  ;; Possible Emacs bug (31.1): line numbers cost much more here than
  ;; in text-mode. Most allocation is in C (redisplay_internal and
  ;; line-move-visual), not in Lisp. A guess: something in
  ;; markdown-ts-mode makes each line move redisplay the whole window,
  ;; so Emacs builds every line number again. If a later Emacs fixes
  ;; this, remove the hook. Candidate for M-x report-emacs-bug.
  :hook (markdown-ts-view-mode . (lambda () (display-line-numbers-mode -1))))

(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-use-buttons 'in-margin)
  :hook ((emacs-lisp-mode . outline-minor-mode)
         ;; Only tree-sitter modes with headings set these variables.
         ;; treesit-major-mode-setup sets the search function. Some
         ;; modes, such as python-ts-mode, set their own regexp instead.
         (after-change-major-mode
          . (lambda ()
              (when (and (treesit-parser-list)
                         (seq-some #'local-variable-p
                                   '(outline-search-function outline-regexp)))
                (outline-minor-mode 1))))))

(use-package lisp-mode
  :ensure nil
  :config
  ;; Lists ;;; and ;;;; outline headings in imenu as a flat group.
  ;; The space after the semicolons skips ;;;###autoload.
  (add-to-list 'lisp-imenu-generic-expression
               '("Sections" "^;;;+ \\(.+\\)$" 1)))

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
       tramp-methods))
  ;; TRAMP replaces the remote PATH with `tramp-remote-path', so a container
  ;; loses PATH entries of its image, such as a venv. Read the PATH of a
  ;; login shell in podman containers only.
  (let* ((own-path (cons 'tramp-own-remote-path tramp-remote-path))
         (variables `((tramp-remote-path . ,own-path))))
    (connection-local-set-profile-variables 'tramp-podman-own-path variables)
    (connection-local-set-profiles
     '(:application tramp :protocol "podman")
     'tramp-podman-own-path)))

(use-package comint
  :ensure nil
  :custom
  (comint-buffer-maximum-size 10000)
  :config
  ;; Shell buffers grow with no limit. Keep only the last lines.
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer))

;; `esh-mode' defines both variables, and `eshell' does not load it.
(use-package esh-mode
  :ensure nil
  :custom
  (eshell-buffer-maximum-lines 10000)
  :config
  ;; Eshell buffers grow with no limit. Keep only the last lines.
  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer))

(use-package simple
  :ensure nil
  :config
  ;; Several commands share "*Async Shell Command*" or
  ;; "*Shell Command Output*".  Name the output buffer after the
  ;; command.  Change only interactive calls, because many Lisp callers
  ;; run `shell-command' with no output buffer and would each leave a
  ;; buffer.  A remote command gets the TRAMP prefix in the name, so the
  ;; buffer shows where the command runs.
  (defun shell-command-name-output-buffer (args)
    (interactive
     (lambda (spec)
       (let ((interactive-args (advice-eval-interactive-spec spec)))
         (pcase-let ((`(,command ,output-buffer . ,rest) interactive-args))
           (if output-buffer
               interactive-args
             (let* ((remote (file-remote-p default-directory))
                    (buffer-name (if remote
                                     (format "*%s* @ %s" command remote)
                                   (format "*%s*" command))))
               `(,command ,buffer-name ,@rest)))))))
    args)
  (advice-add 'shell-command :filter-args #'shell-command-name-output-buffer)
  (advice-add 'async-shell-command :filter-args #'shell-command-name-output-buffer))

(use-package proced
  :ensure nil
  :custom
  (proced-show-remote-processes t))

(use-package mistty
  :ensure t)

(use-package docker
  :ensure t
  :custom
  ;; Podman's `network ls' has no .Scope field.
  (docker-network-columns
   '((:name "Network ID" :width 20 :template "{{ json .ID }}" :sort nil :format nil)
     (:name "Name" :width 50 :template "{{ json .Name }}" :sort nil :format nil)
     (:name "Driver" :width 10 :template "{{ json .Driver }}" :sort nil :format nil))))

;;; Org

(use-package cheatsheet-capture
  :ensure nil
  :load-path "lisp/cheatsheet-capture"
  :commands (cheatsheet-capture-goto-section cheatsheet-capture-row)
  :hook (org-capture-prepare-finalize . cheatsheet-capture-remove-extra-newline))

(use-package org
  :ensure nil
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :custom
  (org-directory "~/Documents/Org")
  ;; Not in org-agenda-files: the cheat sheet has no tasks.  Org reads
  ;; this list only for text search (C-c a s), not for agenda views.
  (org-agenda-text-search-extra-files
   (list (expand-file-name "cheatsheet.org" user-emacs-directory)))
  ;; lisp/cheatsheet-capture/cheatsheet-capture-tests.el tests template
  ;; "k".  The tests read it from this block, so run them after a
  ;; change here.
  (org-capture-templates
   `(("k" "Key binding" table-line
      (file+function
       ,(expand-file-name "cheatsheet.org" user-emacs-directory)
       cheatsheet-capture-goto-section)
      (function cheatsheet-capture-row))))
  :hook (org-mode . visual-line-mode))

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

;;; Late Init

;; envrc must enable after the other global modes, so that its hook runs
;; first and sets the buffer environment before Eglot starts a server.
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))
