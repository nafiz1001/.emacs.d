;;; run-all.el --- Run every ERT test file of this repository -*- lexical-binding: t -*-

;;; Commentary:

;; Do not rename this file to a name that ends in -tests.el.  The
;; runner would then find itself and start copies of itself without
;; end.
;;
;; Runs each of test/*-tests.el and lisp/*/*-tests.el in its own
;; `emacs -Q --batch' process.  Several test files load parts of
;; init.el or add and remove advice, so they must not share an Emacs.
;; Prints the output of each file while it runs, PASS or FAIL per file
;; and a summary.  Exits with 1 if a file fails.
;;
;; Run from any directory:
;;
;;   emacs -Q --batch -l test/run-all.el
;;
;; From Emacs, give the same command to M-x compile.
;;
;; test/eglot-tests.el fetches a debbugs report and can take two
;; minutes.  Without network, its online test skips.

;;; Code:

(unless noninteractive
  (user-error "Run this file with emacs --batch, see its Commentary"))

;; Write the output of a test process to stderr, where `message' also
;; writes, so the lines stay in order.  A filter shows the output while
;; the process runs.  `call-process' with (:file "/dev/stderr") would
;; truncate a log file that stderr is redirected to.
(defun my/run-tests-print-output (_process output)
  (princ output #'external-debugging-output))

(let* ((test-dir (file-name-directory load-file-name))
       (root (file-name-directory (directory-file-name test-dir)))
       ;; The -L and -l paths are relative to the repository root.
       (default-directory root)
       (unit-files (file-expand-wildcards "test/*-tests.el"))
       (package-files (file-expand-wildcards "lisp/*/*-tests.el"))
       (files (append unit-files package-files))
       ;; The same Emacs binary that runs this script.
       (emacs (expand-file-name invocation-name invocation-directory))
       (failed nil))
  (dolist (file files)
    (message "==> %s" file)
    (let* ((file-dir (file-name-directory file))
           ;; -L adds the directory of the file, so a package test can
           ;; require its package.
           (command (list emacs "-Q" "--batch" "-L" file-dir "-l" file
                          "-f" "ert-run-tests-batch-and-exit"))
           ;; With :stderr nil, stderr goes to the filter too.
           (process (make-process :name "run-tests"
                                  :command command
                                  :connection-type 'pipe
                                  :filter #'my/run-tests-print-output
                                  :sentinel #'ignore)))
      ;; Returns nil only after the process exited and its output is
      ;; read.
      (while (accept-process-output process))
      (if (eq (process-exit-status process) 0)
          (message "PASS %s" file)
        (message "FAIL %s" file)
        (push file failed))))
  (let ((total (length files)))
    (if failed
        (let* ((failed-count (length failed))
               (in-order (reverse failed))
               (failed-lines (string-join in-order "\n  ")))
          (message "\n%d of %d test files failed:\n  %s"
                   failed-count total failed-lines))
      (message "\nAll %d test files passed." total)))
  (kill-emacs (if failed 1 0)))

;;; run-all.el ends here
