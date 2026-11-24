;;; editorconfig-tests.el --- Canary tests for editorconfig workarounds -*- lexical-binding: t -*-

;;; Commentary:

;; Canary tests for the init.el block `editorconfig'.  Each test runs
;; Emacs code without the workaround and passes while the upstream bug
;; exists.  If a test fails, upstream fixed the bug: remove the
;; workaround and the test.
;;
;; Run:
;;   emacs -Q --batch -l test/editorconfig-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'editorconfig)
;; Both indent variables must be bound.  The lookup drops unbound ones.
(require 'json-ts-mode)
(require 'js)

(ert-deftest my/editorconfig-tests-upstream-json-ts-indent ()
  "Canary: editorconfig sets `js-indent-level' for json-ts-mode.
json-ts-mode ignores that variable.  If this fails, upstream fixed the
bug.  Remove the `editorconfig-indentation-alist' entry for json-ts-mode
in the init.el block `editorconfig' and this test."
  (with-temp-buffer
    (setq major-mode 'json-ts-mode)
    (let ((settings (editorconfig--default-indent-size-function 2)))
      (should (assq 'js-indent-level settings))
      (should-not (assq 'json-ts-indent-offset settings)))))

;;; editorconfig-tests.el ends here
