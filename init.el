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

;; Keeps the files of packages in etc/ and var/.
(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups))

;; Emacs.app started from the Dock, or a daemon started by launchd, does
;; not get the PATH of the login shell.  Then git, rg, direnv and LSP
;; servers are not found.  Emacs in a terminal inherits the PATH.
;; `when', not :if: use-package installs an :ensure package even if
;; :if is nil.
(when (and (eq system-type 'darwin)
           (or (memq window-system '(mac ns))
               (daemonp)))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;;; Core

;; Built-in Emacs settings: core behavior.
(use-package emacs
  :ensure nil
  :custom
  (mouse-wheel-progressive-speed nil)
  ;; Scroll just enough to keep point visible.
  (scroll-conservatively 101)
  (delete-by-moving-to-trash t)
  (visible-bell t)
  :config
  ;; Enable without a confirmation prompt.
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil))

;; Saves minibuffer history between sessions.
(use-package savehist
  :ensure nil
  :custom
  (savehist-mode t))

;; List of recently opened files.
(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t))

;; Reopens a file at the last point.
(use-package saveplace
  :ensure nil
  :custom
  (save-place-mode t))

;; Reloads buffers when their files change on disk.
(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  ;; Also Dired and other non-file buffers.
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

;; Keeps Emacs fast in files with very long lines.
(use-package so-long
  :ensure nil
  :custom
  (global-so-long-mode t))

;; Repeats some commands with their last key, for example C-x o o.
(use-package repeat
  :ensure nil
  :custom
  (repeat-mode t))

;; Mouse support in terminal frames.
(use-package xt-mouse
  :ensure nil
  :custom
  (xterm-mouse-mode t))

;; System clipboard in terminal frames.
(use-package xclip
  :ensure t
  :config
  ;; xclip-mode signals an error if the clipboard program is missing,
  ;; for example on native Windows or on Linux without xclip.
  (when (executable-find xclip-program)
    (xclip-mode 1)))

;; Draws ASCII art with the mouse or keys.
(use-package artist
  :ensure nil
  :preface
  (defun my/artist-disable-tabs ()
    (setq indent-tabs-mode nil))
  :hook (artist-mode . my/artist-disable-tabs))

;;; UI

;; Built-in Emacs settings: UI.
(use-package emacs
  :ensure nil
  :custom
  (custom-enabled-themes '(modus-vivendi))
  (inhibit-startup-screen t))

;; Basic editing commands and mode line indicators.
(use-package simple
  :ensure nil
  :custom
  (column-number-mode t)
  (size-indication-mode t))

;; Line numbers in buffers.
(use-package display-line-numbers
  :ensure nil
  :custom
  (global-display-line-numbers-mode t))

;; Tool bar of graphical frames.
(use-package tool-bar
  :ensure nil
  :custom
  (tool-bar-mode nil))

;; Tab bar for window configurations.
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-mode t))

;; Shows the keys that can follow a prefix key.
(use-package which-key
  :ensure nil
  :custom
  (which-key-mode t))

;; Mouse commands, including the right-click context menu.
(use-package mouse
  :ensure nil
  :custom
  (context-menu-mode t))

;; Start buffer with recent files, projects and bookmarks.
(use-package dashboard
  :ensure t
  :preface
  (defun my/dashboard-initial-buffer ()
    (dashboard-open)
    (get-buffer dashboard-buffer-name))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents   . 5)
                     (projects  . 10)
                     (bookmarks . 5)))
  ;; `dashboard-setup-startup-hook' skips `emacs --daemon', so client
  ;; frames need this to show the dashboard.
  (initial-buffer-choice #'my/dashboard-initial-buffer)
  :config
  (dashboard-setup-startup-hook))

;;; Completion

;; Minibuffer input and completion.
(use-package minibuffer
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  (completion-styles '(basic partial-completion emacs22 substring flex))
  (completions-sort 'historical)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

;; Completion UI in the minibuffer (Fido).
(use-package icomplete
  :ensure nil
  :custom
  ;; icomplete with flex matching.
  (fido-mode t))

;;; Files

;; File manager.
(use-package dired
  :ensure nil
  :preface
  (defun my/dired-copy-absolute-file-name ()
    (interactive)
    (dired-copy-filename-as-kill 0))
  :custom
  ;; Copy and rename target the other Dired window.
  (dired-dwim-target t)
  (dired-listing-switches "-alh --group-directories-first")
  ;; Keep one Dired buffer.
  (dired-kill-when-opening-new-dired-buffer t)
  :bind (:map dired-mode-map
              ;; Copy the absolute file name.
              ("W" . my/dired-copy-absolute-file-name))
  :config
  ;; Enable `a' without a prompt.
  (put 'dired-find-alternate-file 'disabled nil)
  ;; --group-directories-first needs GNU ls.  The BSD ls of macOS
  ;; signals an error on it.  On macOS, use GNU ls from coreutils (gls)
  ;; if it exists.  Else use ls-lisp, the Lisp emulation of ls.  Native
  ;; Windows has no ls and uses ls-lisp by default.
  (let ((gls (and (eq system-type 'darwin)
                  (executable-find "gls"))))
    (cond
     (gls
      (setopt insert-directory-program gls))
     ((memq system-type '(darwin windows-nt))
      (require 'ls-lisp)
      (setopt ls-lisp-use-insert-directory-program nil
              ls-lisp-dirs-first t
              dired-listing-switches "-alh")))))

;; Rename files and edit permissions as text in Dired.
(use-package wdired
  :ensure nil
  :custom
  (wdired-allow-to-change-permissions t))

;; Extra colors in Dired.
(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

;; Opens very large files in parts.
(use-package vlf
  :ensure t)

;;; Navigation

;; Selects a window by number.
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))

;; Find definitions and references, and search files.
(use-package xref
  :ensure nil
  :custom-face
  (xref-file-header ((t (:weight bold :underline t))))
  :config
  ;; Without rg, keep the default grep.
  (when (executable-find "rg")
    (setopt xref-search-program 'ripgrep)))

;; Jumps to the source of functions and variables.
(use-package find-func
  :ensure nil
  :config
  ;; Packaged builds of Emacs (snap, Arch and others) often have no C
  ;; source.  This checkout of the installed version lets
  ;; find-function jump to C definitions.
  ;; find-function-C-source-directory is a defvar, not a defcustom.
  ;; :custom sets it before find-func loads, and the defvar then
  ;; replaces the value with nil.
  (let ((checkout "~/projects/emacs/src"))
    ;; On a system without the checkout, keep the default.  Then
    ;; find-function asks for the directory and does not signal an
    ;; error.
    (when (file-directory-p checkout)
      (setq find-function-C-source-directory checkout))))

;;; VC and projects

;; Git interface.
(use-package magit
  :ensure t)

;; Opens file versions in an external diff tool such as difft or meld
;; (lisp/difftool).  Tests: lisp/difftool/difftool-tests.el.
(use-package difftool
  :ensure nil
  :load-path "lisp/difftool"
  :commands (difftool-compare difftool-magit-dwim)
  :init
  (with-eval-after-load 'magit-diff
    (transient-append-suffix 'magit-diff "t"
      '("x" "External tool" difftool-magit-dwim))))

;; Project commands (C-x p).
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

;; Marks changed lines in the fringe and in Dired.
(use-package diff-hl
  :ensure t
  :vc (diff-hl :url "https://github.com/dgutov/diff-hl"
               :rev "8dfb5c27ea43b8c7ac1c008bb97090ad4f059285")
  ;; :hook defers loading, but global-diff-hl-mode must start at init.
  :demand t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :custom
  ;; Off in TRAMP buffers.
  (diff-hl-disable-on-remote t)
  :config
  (global-diff-hl-mode))

;;; Programming

;;;; Indentation

;; Built-in Emacs settings: indentation.
;; Precedence: .editorconfig and file variables, then tree-sitter
;; detection, then the default offset of the major mode.
(use-package emacs
  :ensure nil
  :custom
  (indent-tabs-mode nil)
  (tab-width 4))

;; Guesses the indent offset of a file from tree-sitter rules
;; (lisp/ts-indent-guess).
(use-package ts-indent-guess
  :ensure nil
  :load-path "lisp/ts-indent-guess"
  :config
  (ts-indent-guess-mode 1))

;; Tree-sitter mode for C.
;; Detection does not find the brace style. The default GNU style indents
;; a brace on its own line one extra level.
(use-package c-ts-mode
  :ensure nil
  :defer t
  :custom
  ;; In c-ts-mode, k&r and linux also indent a brace on its own line one
  ;; extra level. bsd does not, and it keeps K&R code unchanged.
  (c-ts-mode-indent-style 'bsd))

;; CC mode for C, C++, Java and AWK.
(use-package cc-mode
  :ensure nil
  :defer t
  :custom
  ;; In CC mode, "k&r" sets `c-basic-offset' to 5. "stroustrup" uses 4.
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "stroustrup"))))

;; Python modes.
(use-package python
  :ensure nil
  :preface
  ;; python-ts-mode enables node-based `sexp' navigation. Then
  ;; `backward-sexp' after a closing paren that ends a method moves to
  ;; the `def', not to the matching paren. Use list-based navigation,
  ;; which is the treesit default. The hook sets the same values as
  ;; `treesit-cycle-sexp-thing' gives in list mode. It does not call
  ;; that command, because the command toggles, so a later change of
  ;; the python-ts-mode default would flip the result.
  (defun my/python-ts-use-list-sexp ()
    (setq-local treesit-sexp-thing nil
                forward-sexp-function
                #'treesit-forward-sexp-list))
  :hook (python-ts-mode . my/python-ts-use-list-sexp))

;; Shell script modes.
;; Emacs has no .env mode. Dotenv syntax is a subset of shell.
;; The regexp does not match .envrc.
(use-package sh-script
  :ensure nil
  :mode ("\\.env\\(?:\\..*\\)?\\'" . bash-ts-mode))

;;;; Other

;; Tree-sitter parsing for major modes.
(use-package treesit
  :ensure nil
  :custom
  ;; Use tree-sitter modes for all languages.
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'ask))

;; LSP client.
(use-package eglot
  :ensure nil
  :after (treesit)
  ;; Inlay hints are overlay strings. If a hint wraps at the window edge,
  ;; scrolling gets blocked. The display engine handles wrapped overlay
  ;; strings badly, see:
  ;; https://debbugs.gnu.org/20847
  ;; https://debbugs.gnu.org/25246
  ;; https://debbugs.gnu.org/67470
  ;; Canary test: test/eglot-tests.el
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
  ;; Canary test: test/eglot-tests.el
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

;; Applies .editorconfig settings.
(use-package editorconfig
  :ensure nil
  :config
  ;; The default maps json-ts-mode to js-indent-level, which it ignores.
  ;; Canary test: test/editorconfig-tests.el
  (add-to-list 'editorconfig-indentation-alist
               '(json-ts-mode json-ts-indent-offset))
  (editorconfig-mode 1))

;; Formats buffers on save with external formatters.
(use-package apheleia
  :ensure t
  :preface
  ;; .env files use bash-ts-mode for highlighting only. shfmt can
  ;; change unquoted values, for example a&b to a & b.
  (defun my/apheleia-inhibit-env-file ()
    (and buffer-file-name
         (string-match-p "\\.env\\(?:\\..*\\)?\\'"
                         buffer-file-name)))
  :config
  (add-hook 'apheleia-inhibit-functions #'my/apheleia-inhibit-env-file)
  (apheleia-global-mode))

;; Ctags completion and jump to definition.
(use-package citre
  :ensure t
  :config
  (require 'citre-config))

;; Highlights TODO, FIXME and similar keywords.
(use-package hl-todo
  :ensure t
  :hook (prog-mode))

;; Inserts closing brackets and quotes.
(use-package elec-pair
  :ensure nil
  :custom
  (electric-pair-mode t))

;; Markdown modes.
;; Eglot renders LSP documentation with gfm-view-mode from markdown-mode.
;; Without it, Eglot shows raw markdown in text-mode.
(use-package markdown-mode
  :ensure t
  :preface
  ;; Bug in markdown-mode: if the source buffer of a live preview dies
  ;; first, the preview keeps a pointer to the dead buffer.  The kill
  ;; hook of the preview then calls with-current-buffer on it, which
  ;; signals "Selecting deleted buffer".  An error in kill-buffer-hook
  ;; aborts kill-buffer, so the preview (an eww buffer) cannot be
  ;; killed.  Clear the dead pointer first.
  ;; Canary test: test/markdown-mode-tests.el
  (defun my/markdown-live-preview-clear-dead-source ()
    (unless (buffer-live-p markdown-live-preview-source-buffer)
      (setq markdown-live-preview-source-buffer nil)))
  :config
  (advice-add 'markdown-live-preview-remove-on-kill :before
              #'my/markdown-live-preview-clear-dead-source))

;; Tree-sitter mode for Markdown.
;; markdown-ts-mode does not add itself to auto-mode-alist.
;; This block comes after markdown-mode, so its .md entry wins.
(use-package markdown-ts-mode
  :ensure nil
  ;; Canary test: test/markdown-ts-mode-tests.el
  :mode "\\.md\\'"
  :preface
  ;; Line numbers make each redisplay in this mode allocate about 40%
  ;; more. A read-only view does not need them.
  ;; Possible Emacs bug (31.1): line numbers cost much more here than
  ;; in text-mode. Most allocation is in C (redisplay_internal and
  ;; line-move-visual), not in Lisp. A guess: something in
  ;; markdown-ts-mode makes each line move redisplay the whole window,
  ;; so Emacs builds every line number again. If a later Emacs fixes
  ;; this, remove the hook. Candidate for M-x report-emacs-bug.
  (defun my/markdown-ts-view-hide-line-numbers ()
    (display-line-numbers-mode -1))
  ;; Emacs bug (31.1): a link button gives a relative URL to find-file
  ;; unchanged.  "file.md#frag" opens a new empty buffer
  ;; "file.md#frag", and "my%20file.md" opens "my%20file.md".  Split
  ;; off the fragment, decode the file part, open the file, then jump
  ;; to the heading of the fragment.  The fragment stays encoded: the
  ;; heading slugs of markdown-ts-mode use the same form.
  ;; If a later Emacs fixes this, remove the advice.
  ;; Tests and canary test: test/markdown-ts-link-tests.el
  (defun my/markdown-ts-follow-file-link (button)
    (let* ((url (button-get button 'my/markdown-ts-url))
           (hash (string-search "#" url))
           (encoded-file (substring url 0 hash))
           ;; url-unhex-string returns raw bytes.  Decode them as
           ;; UTF-8, or a non-ASCII character becomes several chars.
           (file-bytes (url-unhex-string encoded-file))
           (file (decode-coding-string file-bytes 'utf-8))
           (fragment (and hash
                          (let ((fragment-start (1+ hash)))
                            (substring url fragment-start)))))
      (find-file file)
      (when (and fragment
                 (not (string-empty-p fragment))
                 (derived-mode-p 'markdown-ts-mode))
        (markdown-ts--follow-fragment fragment))))
  (defun my/markdown-ts-fix-file-link (beg end url)
    (let ((case-fold-search nil))
      (unless (or (string-prefix-p "#" url)
                  (string-match-p "\\`[a-z]+:" url))
        (add-text-properties
         beg end
         `(action my/markdown-ts-follow-file-link
                  my/markdown-ts-url ,url)))))
  :hook (markdown-ts-view-mode . my/markdown-ts-view-hide-line-numbers)
  :config
  (advice-add 'markdown-ts--make-link-button :after
              #'my/markdown-ts-fix-file-link))

;; Folds buffers by headings.
(use-package outline
  :ensure nil
  :preface
  ;; Only tree-sitter modes with headings set these variables.
  ;; treesit-major-mode-setup sets the search function. Some
  ;; modes, such as python-ts-mode, set their own regexp instead.
  (defun my/outline-enable-for-treesit-headings ()
    (when (and (treesit-parser-list)
               (seq-some #'local-variable-p
                         '(outline-search-function outline-regexp)))
      (outline-minor-mode 1)))
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-use-buttons 'in-margin)
  :hook ((emacs-lisp-mode . outline-minor-mode)
         (after-change-major-mode . my/outline-enable-for-treesit-headings)))

;; Lisp modes.
(use-package lisp-mode
  :ensure nil
  :config
  ;; Lists ;;; and ;;;; outline headings in imenu as a flat group.
  ;; The space after the semicolons skips ;;;###autoload.
  (add-to-list 'lisp-imenu-generic-expression
               '("Sections" "^;;;+ \\(.+\\)$" 1)))

;;; Remote and shells

;; Edit remote files and run remote commands.
(use-package tramp
  :ensure nil
  :custom
  ;; Do not reuse SSH ControlMaster connections.
  (tramp-use-connection-share nil)
  :config
  ;; configurations for flatpak Emacs
  ;; flatpak-spawn and toolbox exist only on Linux.
  (when (and (eq system-type 'gnu/linux)
             (not (assoc "toolbox" tramp-methods)))
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

;; Base mode for shell and REPL buffers.
(use-package comint
  :ensure nil
  :custom
  (comint-buffer-maximum-size 10000)
  :config
  ;; Shell buffers grow with no limit. Keep only the last lines.
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer))

;; Eshell buffers.
;; `esh-mode' defines both variables, and `eshell' does not load it.
(use-package esh-mode
  :ensure nil
  :custom
  (eshell-buffer-maximum-lines 10000)
  :config
  ;; Eshell buffers grow with no limit. Keep only the last lines.
  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer))

;; Shell command output buffers.
(use-package simple
  :ensure nil
  :preface
  ;; Several commands share "*Async Shell Command*" or
  ;; "*Shell Command Output*".  Name the output buffer after the
  ;; command.  Change only interactive calls, because many Lisp callers
  ;; run `shell-command' with no output buffer and would each leave a
  ;; buffer.  A remote command gets the TRAMP prefix in the name, so the
  ;; buffer shows where the command runs.
  (defun my/shell-command-output-buffer-args (spec)
    (let ((interactive-args (advice-eval-interactive-spec spec)))
      (pcase-let ((`(,command ,output-buffer . ,rest) interactive-args))
        (if output-buffer
            interactive-args
          (let* ((remote (file-remote-p default-directory))
                 (buffer-name (if remote
                                  (format "*%s* @ %s" command remote)
                                (format "*%s*" command))))
            `(,command ,buffer-name ,@rest))))))
  (defun my/shell-command-name-output-buffer (args)
    (interactive #'my/shell-command-output-buffer-args)
    args)
  :config
  (advice-add 'shell-command :filter-args #'my/shell-command-name-output-buffer)
  (advice-add 'async-shell-command :filter-args #'my/shell-command-name-output-buffer))

;; Process list, like top.
(use-package proced
  :ensure nil
  :custom
  ;; Show processes of the remote host in a TRAMP directory.
  (proced-show-remote-processes t))

;; Terminal for shells that keeps normal Emacs editing.
;; MisTTY needs a pty, which native Windows Emacs does not have.
;; `unless', not :if: use-package installs an :ensure package even if
;; :if is nil.
(unless (eq system-type 'windows-nt)
  (use-package mistty
    :ensure t))

;; Manages Docker and Podman containers.
(use-package docker
  :ensure t
  :custom
  ;; Podman's `network ls' has no .Scope field.
  (docker-network-columns
   '((:name "Network ID" :width 20 :template "{{ json .ID }}" :sort nil :format nil)
     (:name "Name" :width 50 :template "{{ json .Name }}" :sort nil :format nil)
     (:name "Driver" :width 10 :template "{{ json .Driver }}" :sort nil :format nil))))

;;; Org

;; Org capture of key bindings into cheatsheet.org
;; (lisp/cheatsheet-capture).
(use-package cheatsheet-capture
  :ensure nil
  :load-path "lisp/cheatsheet-capture"
  :commands (cheatsheet-capture-goto-section cheatsheet-capture-row)
  :hook (org-capture-prepare-finalize . cheatsheet-capture-remove-extra-newline))

;; Notes, tasks and agenda.
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

;; Commands to run the Freezeman backend, frontend and shell
;; (lisp/freezeman).
(use-package freezeman
  :ensure nil
  :load-path "lisp/freezeman"
  :commands (freezeman-backend freezeman-frontend freezeman-shell))

;;; Late Init

;; Loads the direnv environment per buffer.
;; envrc must enable after the other global modes, so that its hook runs
;; first and sets the buffer environment before Eglot starts a server.
(use-package envrc
  :ensure t
  :preface
  ;; envrc needs the direnv program.  Native Windows and a new macOS
  ;; install do not have it.  `envrc-direnv-executable' is not bound
  ;; yet, because envrc loads only when the mode starts.
  (defun my/envrc-enable-if-direnv ()
    (when (executable-find "direnv")
      (envrc-global-mode 1)))
  :hook (after-init . my/envrc-enable-if-direnv))
