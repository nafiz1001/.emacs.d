;;; difftool.el --- Open file versions in an external diff tool -*- lexical-binding: t -*-

;; Copyright (C) 2026 Nafiz Islam

;; Author: Nafiz Islam
;;         Claude Sonnet 5.5 <noreply@anthropic.com>
;; Maintainer: Nafiz Islam
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Compare file versions of a Git repository in an external diff tool,
;; such as difft (Difftastic) or meld.  The program can be any name or
;; path.
;;
;; Each revision side becomes a temporary file.  A working-tree side is
;; the real file, so edits made in the tool reach the checkout.  The
;; terminal output of the tool goes to a buffer, with ANSI colours.  The
;; temporary files are deleted when the buffer is killed, when the tool
;; exits, and when Emacs exits.
;;
;; Usage:
;;
;;   M-x difftool-compare       Read a program and 2 sides (3 with C-u).
;;   M-x difftool-magit-dwim    Guess the sides from the Magit buffer.
;;
;; In the output buffer, `g' starts the tool again and `q' quits.
;;
;; A side is a cons (REV . FILE).  FILE is relative to the top directory
;; of the repository.  REV is a commit-ish string, `:index' for the
;; index, or nil for the working tree.
;;
;; Limitations:
;;
;; - Git only.  Remote (TRAMP) repositories are not supported.
;; - A tool that forks and exits at once, such as a Meld that hands the
;;   files to a running instance, makes the sentinel delete the
;;   temporary files too early.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'subr-x)
(require 'vc-git)

(declare-function magit-diff--dwim "magit-diff")
(declare-function magit-diff-read-range-or-commit "magit-diff")
(declare-function magit-current-file "magit-git")
(declare-function magit-split-range "magit-git")
(declare-function magit-rev-verify "magit-git")
(declare-function magit-toplevel "magit-git")
(declare-function magit-ediff-read-files "magit-ediff")

(defgroup difftool nil
  "Open file versions in an external diff tool."
  :group 'tools
  :prefix "difftool-")

(defcustom difftool-programs '("difft" "meld")
  "Programs offered when a command asks for a diff tool.
The prompt accepts any other program name or path."
  :type '(repeat string))

(defvar difftool-program-history nil
  "History of the programs that the user chose.
The last one is the default of the next prompt.")

(defvar-local difftool--temp-dir nil
  "Temporary directory of the revision sides of this buffer.")

(defvar-local difftool--process nil
  "Process of the diff tool of this buffer.")

(defvar-local difftool--args nil
  "List (PROGRAM ROOT SIDES) of the last run, for `revert-buffer'.")

;;;; Temporary files

(defun difftool--delete-temp-dir (dir)
  "Delete the directory DIR if it exists.  DIR can be nil."
  (when (and dir (file-directory-p dir))
    (delete-directory dir t)))

(defun difftool--write-blob (root rev file dest)
  "Write FILE of REV in the repository ROOT to the file DEST.
REV is a commit-ish string or `:index'.  Signal a `user-error' if Git
fails."
  (let* ((default-directory root)
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (spec (concat (if (eq rev :index) "" rev) ":" file))
         (dest-dir (file-name-directory dest)))
    (make-directory dest-dir t)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((status (call-process "git" nil t nil "cat-file" "--filters" spec)))
        (unless (eq status 0)
          (let* ((output (decode-coding-string (buffer-string) 'utf-8))
                 (message (string-trim output)))
            (user-error "Git cannot read %s: %s" spec message))))
      (write-region nil nil dest nil 'silent))))

(defun difftool--side-path (root temp-dir index side)
  "Return the file path that the tool reads for SIDE.
SIDE is (REV . FILE) as in the Commentary.  For a revision side, write
the blob to a file in TEMP-DIR.  INDEX makes the path unique.  The file
keeps its basename, so that the tool detects the language."
  (let ((rev (car side))
        (file (cdr side)))
    (if (null rev)
        (expand-file-name file root)
      (let* ((label (if (eq rev :index)
                        "index"
                      (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" rev)))
             (subdir (format "%d-%s" index label))
             (side-dir (expand-file-name subdir temp-dir))
             (basename (file-name-nondirectory file))
             (dest (expand-file-name basename side-dir)))
        (difftool--write-blob root rev file dest)
        dest))))

;;;; Process and buffer lifecycle

(defun difftool--filter (process string)
  "Insert STRING, output of PROCESS, with ANSI colours."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (mark (process-mark process))
              (following (= (point) (process-mark process)))
              (start nil))
          (save-excursion
            (goto-char mark)
            (setq start (point))
            (insert string)
            (ansi-color-apply-on-region start (point))
            (set-marker mark (point)))
          (when following
            (goto-char mark)))))))

(defun difftool--sentinel (process event)
  "Delete the temporary files when PROCESS ends and note EVENT."
  (unless (process-live-p process)
    (let ((temp-dir (process-get process 'difftool-temp-dir))
          (buffer (process-buffer process)))
      (difftool--delete-temp-dir temp-dir)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let* ((inhibit-read-only t)
                 (command (process-command process))
                 (name (file-name-nondirectory (car command)))
                 (status (process-exit-status process))
                 (result (cond ((not (eq (process-status process) 'exit))
                                (string-trim event))
                               ((zerop status) "finished")
                               (t (format "exited %d" status)))))
            (save-excursion
              (goto-char (point-max))
              (unless (bolp)
                (insert "\n"))
              (insert (format "[%s %s]\n" name result)))))))))

(defun difftool--stop ()
  "Stop the tool of the current buffer and delete its temporary files.
Safe to call more than once."
  (let ((process difftool--process))
    (when (process-live-p process)
      ;; The sentinel must not write into a buffer that is being erased or
      ;; killed.
      (set-process-sentinel process #'ignore)
      (delete-process process))
    (difftool--delete-temp-dir difftool--temp-dir)))

(defun difftool--cleanup-all ()
  "Delete the temporary files of all output buffers.
Emacs does not run `kill-buffer-hook' at exit."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'difftool-output-mode)
        (difftool--delete-temp-dir difftool--temp-dir)))))

(defun difftool--rerun (&rest _)
  "Start the tool again with the arguments of the last run.
This is `revert-buffer-function' of `difftool-output-mode'."
  (pcase-let ((`(,program ,root ,sides) difftool--args))
    (difftool--stop)
    (difftool--start (current-buffer) program root sides)))

(define-derived-mode difftool-output-mode special-mode "Difftool"
  "Major mode for the output buffer of an external diff tool.
\\<difftool-output-mode-map>\\[revert-buffer] starts the tool again."
  (setq-local revert-buffer-function #'difftool--rerun)
  (add-hook 'kill-buffer-hook #'difftool--stop nil t))

(defun difftool--start (buffer program root sides)
  "Run PROGRAM on SIDES of the repository ROOT, with output in BUFFER.
BUFFER is in `difftool-output-mode'.  Erase it first."
  (let ((temp-dir (make-temp-file "difftool-" t))
        (started nil))
    (unwind-protect
        (let* ((index 0)
               (paths (mapcar (lambda (side)
                                (setq index (1+ index))
                                (difftool--side-path root temp-dir index side))
                              sides)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              ;; `ansi-color' puts the colours on overlays.
              (delete-all-overlays))
            (setq default-directory root)
            (let* ((window (display-buffer buffer))
                   (width (if window (window-body-width window) 80))
                   (width-setting (format "DFT_WIDTH=%d" width))
                   ;; Difftastic turns colour off on a pipe, unless asked.
                   (process-environment (append (list "DFT_COLOR=always"
                                                      width-setting)
                                                process-environment))
                   (process (make-process :name program
                                          :buffer buffer
                                          :command (cons program paths)
                                          :connection-type 'pipe
                                          ;; Killing the buffer is the way
                                          ;; to stop the tool.
                                          :noquery t
                                          :filter #'difftool--filter
                                          :sentinel #'difftool--sentinel)))
              (process-put process 'difftool-temp-dir temp-dir)
              (setq difftool--process process
                    difftool--temp-dir temp-dir
                    difftool--args (list program root sides))))
          (setq started t))
      (unless started
        (difftool--delete-temp-dir temp-dir)))))

(defun difftool-run (program root sides)
  "Run the external PROGRAM on SIDES of the Git repository ROOT.
SIDES is a list of (REV . FILE) as in the Commentary.  Show the output
in a new buffer and return it."
  (when (file-remote-p root)
    (user-error "Remote repositories are not supported"))
  (let* ((basename (file-name-nondirectory (cdr (car (last sides)))))
         (buffer (generate-new-buffer (format "*difftool %s*" basename)))
         (ok nil))
    (add-hook 'kill-emacs-hook #'difftool--cleanup-all)
    (with-current-buffer buffer
      (difftool-output-mode))
    (unwind-protect
        (progn
          (difftool--start buffer program root sides)
          (setq ok t))
      (unless ok
        (kill-buffer buffer)))
    buffer))

;;;; Commands

(defun difftool--read-program ()
  "Read the name or path of a diff tool."
  (let ((default (or (car difftool-program-history)
                     (car difftool-programs))))
    (completing-read "Diff tool: " difftool-programs nil nil nil
                     'difftool-program-history default)))

(defun difftool--git-root ()
  "Return the top directory of the Git repository of `default-directory'."
  (or (vc-git-root default-directory)
      (user-error "Not in a Git repository")))

(defun difftool--read-side (root index)
  "Read side number INDEX in the repository ROOT as (REV . FILE)."
  (let* ((rev-prompt (format "Revision of side %d (empty: working tree, \":\": index): "
                             index))
         (input (read-string rev-prompt))
         (rev (cond ((string-empty-p input) nil)
                    ((equal input ":") :index)
                    (t input)))
         (file-prompt (format "File of side %d: " index))
         (absolute (read-file-name file-prompt root))
         (relative (file-relative-name absolute root)))
    (when (string-prefix-p ".." relative)
      (user-error "File is outside the repository: %s" absolute))
    (cons rev relative)))

(defun difftool-compare (&optional three-way)
  "Compare file versions of the current Git repository in an external tool.
Read the program, then 2 sides.  With prefix argument THREE-WAY, read 3
sides.  For each side, read a revision and a file.  The file need not
exist in the working tree, so this works across renames."
  (interactive "P")
  (let* ((root (difftool--git-root))
         (program (difftool--read-program))
         (count (if three-way 3 2))
         (sides (cl-loop for index from 1 to count
                         collect (difftool--read-side root index))))
    (difftool-run program root sides)))

(defun difftool--magit-endpoints (range)
  "Return (REV-A . REV-B) for the Magit diff RANGE, or nil if unknown.
RANGE is a value of `magit-diff--dwim'.  A REV is a string, `:index' or
nil for the working tree."
  (pcase range
    ('unstaged (cons :index nil))
    ('staged (cons "HEAD" :index))
    ('unmerged (user-error "Merge conflicts are not supported yet"))
    (`(commit . ,commit)
     (let ((parent (concat commit "^")))
       (unless (magit-rev-verify parent)
         (user-error "Commit %s has no parent" commit))
       (cons parent commit)))
    (`(stash . ,stash) (cons (concat stash "^1") stash))
    ((pred stringp)
     (let ((split (magit-split-range range)))
       (if split
           (cons (car split) (cdr split))
         (cons range nil))))))

(defun difftool-magit-dwim ()
  "Compare the file, commit or range at point in an external tool.
This mirrors `magit-ediff-dwim'.  Read the program last."
  (interactive)
  (require 'magit)
  ;; Magit does not load magit-ediff with the rest, and
  ;; `magit-ediff-read-files' has no autoload.
  (require 'magit-ediff)
  (let* ((range (magit-diff--dwim))
         (file (magit-current-file))
         (endpoints (or (difftool--magit-endpoints range)
                        (let ((input (magit-diff-read-range-or-commit
                                      "Compare range or commit")))
                          (difftool--magit-endpoints input))))
         (rev-a (car endpoints))
         (rev-b (cdr endpoints))
         ;; Magit reads files for a revision pair, and nil is the working
         ;; tree.  The index has the same file names as the working tree.
         (ediff-rev-a (unless (eq rev-a :index) rev-a))
         (ediff-rev-b (unless (eq rev-b :index) rev-b))
         ;; With a working-tree side, `magit-ediff-read-files' ignores the
         ;; file at point and prompts.  Staged and unstaged diffs have the
         ;; same file on both sides, so skip it for them.
         (files (if (and file (memq range '(staged unstaged)))
                    (list file file)
                  (magit-ediff-read-files ediff-rev-a ediff-rev-b file)))
         (root (magit-toplevel))
         (sides (list (cons rev-a (nth 0 files))
                      (cons rev-b (nth 1 files))))
         (program (difftool--read-program)))
    (difftool-run program root sides)))

(provide 'difftool)
;;; difftool.el ends here
