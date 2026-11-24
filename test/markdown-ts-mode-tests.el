;;; markdown-ts-mode-tests.el --- Canary tests for markdown-ts-mode workarounds -*- lexical-binding: t -*-

;;; Commentary:

;; Canary tests for the init.el block `markdown-ts-mode'.  Each test
;; runs Emacs code without the workaround and passes while the upstream
;; problem exists.  If a test fails, upstream fixed the problem: remove
;; the workaround and the test.
;;
;; Not tested: the hook `my/markdown-ts-view-hide-line-numbers'.  It
;; works around a possible redisplay cost of line numbers, and batch
;; Emacs has no redisplay.  After an Emacs upgrade, check it by hand: in
;; a markdown-ts-view-mode buffer, compare `memory-use-counts' over many
;; line moves with and without `display-line-numbers-mode'.
;;
;; Run:
;;   emacs -Q --batch -l test/markdown-ts-mode-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'markdown-ts-mode)

(ert-deftest my/markdown-ts-mode-tests-upstream-no-auto-mode ()
  "Canary: markdown-ts-mode does not add itself to `auto-mode-alist'.
If this fails, upstream maps .md files to markdown-ts-mode.  Remove the
:mode keyword of the init.el block `markdown-ts-mode' and this test."
  (let ((md-mode (assoc-default "x.md" auto-mode-alist #'string-match)))
    (should-not (eq md-mode 'markdown-ts-mode))
    (should-not (rassq 'markdown-ts-mode auto-mode-alist))
    (should-not (rassq 'markdown-ts-mode major-mode-remap-defaults))))

;;; markdown-ts-mode-tests.el ends here
