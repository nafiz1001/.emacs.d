;;; difftool-tests.el --- Tests for difftool -*- lexical-binding: t -*-

;; Copyright (C) 2026 Nafiz Islam

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

;; ERT tests for difftool.el.
;;
;; Run all tests from the repository root:
;;
;;   emacs -Q --batch -L lisp/difftool \
;;         -l lisp/difftool/difftool-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or, in a running Emacs, load this file and type M-x ert RET t RET.
;; The selector t runs all loaded tests.  A string selector, such as
;; "difftool-test-ansi", runs only the tests whose name matches.
;;
;; The tests of the Magit mapping are skipped if Magit does not load.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'difftool)

;;;; Helpers

(defun difftool-tests--git (dir &rest args)
  "Run git with ARGS in DIR.  Fail the test if git fails."
  (let ((status (apply #'call-process "git" nil nil nil
                       "-C" dir
                       "-c" "user.name=Test" "-c" "user.email=test@example.com"
                       "-c" "commit.gpgsign=false"
                       args)))
    (should (eq status 0))))

(defun difftool-tests--write (path content)
  "Write CONTENT to PATH, creating the directory."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defun difftool-tests--script (dir name body)
  "Write the executable shell script NAME with BODY in DIR.  Return its path."
  (let ((path (expand-file-name name dir)))
    (difftool-tests--write path (concat "#!/bin/sh\n" body "\n"))
    (set-file-modes path #o755)
    path))

(defun difftool-tests--wait-until (predicate)
  "Wait until PREDICATE returns non-nil, at most 10 seconds.  Return its value."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun difftool-tests--leftovers ()
  "Return the difftool directories in `temporary-file-directory'."
  (directory-files temporary-file-directory nil "\\`difftool-"))

(defmacro difftool-tests--with-repo (&rest body)
  "Run BODY in a temporary Git repository.
Variables: `root' is the top directory and `tools' is a directory for
stub programs.  File src/foo.py has the content \"one\" in HEAD~1,
\"two\" in HEAD and \"three\" in the working tree.  Afterward, kill the
output buffers and delete everything."
  (declare (indent 0) (debug t))
  `(ert-with-temp-directory base
     (let* ((root (file-name-as-directory (expand-file-name "repo" base)))
            (tools (expand-file-name "tools" base))
            (temporary-file-directory (expand-file-name "tmp" base))
            (file (expand-file-name "src/foo.py" root))
            (default-directory root))
       (make-directory temporary-file-directory t)
       (make-directory root t)
       (make-directory tools t)
       (difftool-tests--git root "init" "-q")
       (difftool-tests--write file "one\n")
       (difftool-tests--git root "add" "src/foo.py")
       (difftool-tests--git root "commit" "-q" "-m" "first")
       (difftool-tests--write file "two\n")
       (difftool-tests--git root "commit" "-q" "-a" "-m" "second")
       (difftool-tests--write file "three\n")
       (unwind-protect
           (progn ,@body)
         (dolist (buffer (buffer-list))
           (when (eq (buffer-local-value 'major-mode buffer)
                     'difftool-output-mode)
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buffer))))))))

;;;; Core

(ert-deftest difftool-test-revision-and-worktree-sides ()
  "A revision side is a temporary file, and the worktree side is real."
  (difftool-tests--with-repo
    (let* ((sides (list (cons "HEAD~1" "src/foo.py") (cons nil "src/foo.py")))
           (buffer (difftool-run "cat" root sides))
           (temp-dir (buffer-local-value 'difftool--temp-dir buffer))
           (process (buffer-local-value 'difftool--process buffer))
           (command (process-command process)))
      (should (difftool-tests--wait-until
               (lambda ()
                 (with-current-buffer buffer
                   (string-match-p "\\[cat finished\\]" (buffer-string))))))
      (with-current-buffer buffer
        (should (string-match-p "\\`one\nthree\n" (buffer-string))))
      (should (equal (nth 2 command) file))
      (should-not (file-exists-p temp-dir))
      (should-not (difftool-tests--leftovers)))))

(ert-deftest difftool-test-index-side ()
  "The index side holds the staged content."
  (difftool-tests--with-repo
    (difftool-tests--git root "add" "src/foo.py")
    (difftool-tests--write file "four\n")
    (let* ((sides (list (cons "HEAD" "src/foo.py") (cons :index "src/foo.py")))
           (buffer (difftool-run "cat" root sides)))
      (should (difftool-tests--wait-until
               (lambda ()
                 (with-current-buffer buffer
                   (string-match-p "\\[cat finished\\]" (buffer-string))))))
      (with-current-buffer buffer
        (should (string-match-p "\\`two\nthree\n" (buffer-string)))))))

(ert-deftest difftool-test-temp-files-keep-basename ()
  "A temporary file keeps the basename, in a directory with the revision."
  (difftool-tests--with-repo
    (let* ((script (difftool-tests--script tools "echo-args" "printf '%s\\n' \"$@\""))
           (sides (list (cons "HEAD~1" "src/foo.py") (cons :index "src/foo.py")))
           (buffer (difftool-run script root sides)))
      (should (difftool-tests--wait-until
               (lambda ()
                 (with-current-buffer buffer
                   (string-match-p "echo-args finished" (buffer-string))))))
      (with-current-buffer buffer
        (let ((lines (split-string (buffer-string) "\n" t)))
          (should (string-match-p "/1-HEAD_1/foo\\.py\\'" (nth 0 lines)))
          (should (string-match-p "/2-index/foo\\.py\\'" (nth 1 lines))))))))

(ert-deftest difftool-test-ansi-colours ()
  "ANSI escape sequences become text properties."
  (difftool-tests--with-repo
    (let* ((script (difftool-tests--script
                    tools "red" "printf '\\033[31mred\\033[0m\\n'"))
           (sides (list (cons "HEAD" "src/foo.py") (cons nil "src/foo.py")))
           (buffer (difftool-run script root sides)))
      (should (difftool-tests--wait-until
               (lambda ()
                 (with-current-buffer buffer
                   (string-match-p "red finished" (buffer-string))))))
      (with-current-buffer buffer
        (should (string-prefix-p "red\n" (buffer-string)))
        ;; `ansi-color' puts the face on an overlay.
        (let ((overlays (overlays-at 1)))
          (should overlays)
          (should (overlay-get (car overlays) 'face)))))))

(ert-deftest difftool-test-environment ()
  "The tool gets DFT_COLOR and DFT_WIDTH."
  (difftool-tests--with-repo
    (let* ((script (difftool-tests--script
                    tools "env" "echo \"$DFT_COLOR $DFT_WIDTH\""))
           (sides (list (cons "HEAD" "src/foo.py") (cons nil "src/foo.py")))
           (buffer (difftool-run script root sides)))
      (should (difftool-tests--wait-until
               (lambda ()
                 (with-current-buffer buffer
                   (string-match-p "env finished" (buffer-string))))))
      (with-current-buffer buffer
        (should (string-match-p "\\`always [0-9]+\n" (buffer-string)))))))

(ert-deftest difftool-test-kill-buffer-stops-tool ()
  "Killing the buffer stops the tool and deletes the temporary files."
  (difftool-tests--with-repo
    (let* ((script (difftool-tests--script tools "slow" "sleep 30"))
           (sides (list (cons "HEAD~1" "src/foo.py") (cons nil "src/foo.py")))
           (buffer (difftool-run script root sides))
           (process (buffer-local-value 'difftool--process buffer))
           (temp-dir (buffer-local-value 'difftool--temp-dir buffer)))
      (should (process-live-p process))
      (should (file-directory-p temp-dir))
      (kill-buffer buffer)
      (should-not (process-live-p process))
      (should-not (file-exists-p temp-dir)))))

(ert-deftest difftool-test-cleanup-all ()
  "The exit hook deletes the temporary files of a running tool."
  (difftool-tests--with-repo
    (let* ((script (difftool-tests--script tools "slow" "sleep 30"))
           (sides (list (cons "HEAD~1" "src/foo.py") (cons nil "src/foo.py")))
           (buffer (difftool-run script root sides))
           (temp-dir (buffer-local-value 'difftool--temp-dir buffer)))
      (should (file-directory-p temp-dir))
      (difftool--cleanup-all)
      (should-not (file-exists-p temp-dir)))))

(ert-deftest difftool-test-revert-runs-again ()
  "Reverting the buffer starts the tool again with new temporary files."
  (difftool-tests--with-repo
    (let* ((script (difftool-tests--script tools "slow" "sleep 30"))
           (sides (list (cons "HEAD~1" "src/foo.py") (cons nil "src/foo.py")))
           (buffer (difftool-run script root sides))
           (old-process (buffer-local-value 'difftool--process buffer))
           (old-dir (buffer-local-value 'difftool--temp-dir buffer)))
      (with-current-buffer buffer
        (revert-buffer t t))
      (let ((new-process (buffer-local-value 'difftool--process buffer))
            (new-dir (buffer-local-value 'difftool--temp-dir buffer)))
        (should-not (process-live-p old-process))
        (should-not (file-exists-p old-dir))
        (should (process-live-p new-process))
        (should-not (equal old-dir new-dir))
        (should (file-exists-p (expand-file-name "1-HEAD_1/foo.py" new-dir)))))))

(ert-deftest difftool-test-bad-revision ()
  "A revision that does not exist signals an error and leaves nothing."
  (difftool-tests--with-repo
    (let ((sides (list (cons "no-such-rev" "src/foo.py") (cons nil "src/foo.py"))))
      (should-error (difftool-run "cat" root sides) :type 'user-error)
      (should-not (difftool-tests--leftovers))
      (should-not (seq-filter (lambda (buffer)
                                (string-prefix-p "*difftool" (buffer-name buffer)))
                              (buffer-list))))))

(ert-deftest difftool-test-missing-program ()
  "A program that does not exist signals an error and leaves nothing."
  (difftool-tests--with-repo
    (let ((sides (list (cons "HEAD" "src/foo.py") (cons nil "src/foo.py"))))
      (should-error (difftool-run "difftool-no-such-program" root sides))
      (should-not (difftool-tests--leftovers))
      (should-not (seq-filter (lambda (buffer)
                                (string-prefix-p "*difftool" (buffer-name buffer)))
                              (buffer-list))))))

(ert-deftest difftool-test-remote-root ()
  "A remote repository signals a user error."
  (should-error (difftool-run "cat" "/ssh:example.invalid:/repo/"
                              (list (cons nil "a") (cons nil "b")))
                :type 'user-error))

;;;; Magit

(defconst difftool-tests--elpa-dir
  (expand-file-name "../../elpa/" (file-name-directory
                                   (or load-file-name buffer-file-name)))
  "The elpa directory of this configuration, where Magit lives.")

(defun difftool-tests--load-magit ()
  "Load Magit from the elpa directory.  Return nil if it does not load."
  (let ((package-user-dir difftool-tests--elpa-dir))
    (package-initialize)
    (require 'magit nil t)))

(ert-deftest difftool-test-magit-endpoints ()
  "The Magit diff ranges map to the expected revisions."
  (skip-unless (difftool-tests--load-magit))
  (difftool-tests--with-repo
    (should (equal (difftool--magit-endpoints 'unstaged) '(:index)))
    (should (equal (difftool--magit-endpoints 'staged) '("HEAD" . :index)))
    (should (equal (difftool--magit-endpoints '(commit . "HEAD"))
                   '("HEAD^" . "HEAD")))
    (should-error (difftool--magit-endpoints '(commit . "HEAD~1"))
                  :type 'user-error)
    (should (equal (difftool--magit-endpoints '(stash . "stash@{0}"))
                   '("stash@{0}^1" . "stash@{0}")))
    (should (equal (difftool--magit-endpoints "HEAD~1..HEAD")
                   '("HEAD~1" . "HEAD")))
    (should (equal (difftool--magit-endpoints "HEAD") '("HEAD")))
    (should-error (difftool--magit-endpoints 'unmerged) :type 'user-error)
    (should-not (difftool--magit-endpoints 'undefined))))

(provide 'difftool-tests)
;;; difftool-tests.el ends here
