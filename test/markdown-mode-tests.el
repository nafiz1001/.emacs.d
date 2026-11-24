;;; markdown-mode-tests.el --- Canary tests for markdown-mode workarounds -*- lexical-binding: t -*-

;;; Commentary:

;; Canary tests for the init.el block `markdown-mode'.  Each test runs
;; package code without the workaround and passes while the upstream
;; bug exists.  If a test fails, upstream fixed the bug: remove the
;; workaround and the test.
;;
;; The tests load markdown-mode from elpa/ of this repo.
;;
;; Run:
;;   emacs -Q --batch -l test/markdown-mode-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'package)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name test-dir))))
  (setq package-user-dir (expand-file-name "elpa" root))
  (package-initialize))

(require 'markdown-mode nil t)

(ert-deftest my/markdown-mode-tests-upstream-preview-dead-source ()
  "Canary: the kill hook of a live preview fails if its source is dead.
If this fails, upstream fixed the bug.  Remove the advice
`my/markdown-live-preview-clear-dead-source' in the init.el block
`markdown-mode' and this test."
  (skip-unless (featurep 'markdown-mode))
  (let ((source (generate-new-buffer "source.md"))
        (preview (generate-new-buffer "*preview*")))
    (unwind-protect
        (with-current-buffer preview
          (setq markdown-live-preview-source-buffer source)
          (kill-buffer source)
          (should-error (markdown-live-preview-remove-on-kill)))
      ;; The bug also aborts `kill-buffer' from the kill hook.  Clear
      ;; the pointer, so the cleanup can kill the preview.
      (with-current-buffer preview
        (setq markdown-live-preview-source-buffer nil))
      (kill-buffer preview))))

;;; markdown-mode-tests.el ends here
