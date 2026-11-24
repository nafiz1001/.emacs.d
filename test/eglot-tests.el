;;; eglot-tests.el --- Canary tests for Eglot workarounds -*- lexical-binding: t -*-

;;; Commentary:

;; Canary tests for the init.el block `eglot'.  Each test runs Emacs
;; code without the workaround and passes while the upstream bug
;; exists.  If a test fails, upstream fixed the bug: remove the
;; workaround and the test.
;;
;; The test of the debbugs report needs the network.  Without it, the
;; test skips.
;;
;; Run:
;;   emacs -Q --batch -l test/eglot-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'eglot)
(require 'url)

(defun my/eglot-tests--range (line)
  "Return an LSP range from the start to the end of LINE (0-based)."
  `(:start (:line ,line :character 0) :end (:line ,line :character 3)))

(ert-deftest my/eglot-tests-upstream-imenu-parent-position ()
  "Canary: Eglot's imenu index drops the position of a parent symbol.
If this fails, upstream fixed the bug.  Remove `imenu' from
`eglot-stay-out-of' in the init.el block `eglot' and this test."
  (should (fboundp 'eglot--imenu-DocumentSymbol))
  (with-temp-buffer
    (insert "foo\n\nbar\n")
    (let* ((child `(:name "bar" :kind 6
                    :range ,(my/eglot-tests--range 2)
                    :selectionRange ,(my/eglot-tests--range 2)))
           (parent `(:name "foo" :kind 5
                     :range ,(my/eglot-tests--range 0)
                     :selectionRange ,(my/eglot-tests--range 0)
                     :children ,(vector child)))
           (symbols (vector parent))
           (index (eglot--imenu-DocumentSymbol symbols))
           (entry (assoc "foo" index))
           (subitems (cdr entry)))
      ;; Without a position, the entry is a submenu with only the
      ;; children.  imenu cannot jump to the parent itself.
      (should (consp subitems))
      (should-not (rassoc 1 subitems)))))

;; Bugs 20847 and 25246, also named in init.el, are closed already, so
;; only 67470 can tell when the workaround is no longer needed.
(defconst my/eglot-tests-wrapped-overlay-bug 67470
  "Debbugs number of the wrapped overlay string bug.")

(defun my/eglot-tests--bug-report (number)
  "Return the HTML report of debbugs bug NUMBER, or nil without network."
  (let* ((url (format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%d"
                      number))
         ;; debbugs.gnu.org often takes 40 seconds or more to answer.
         (buffer (ignore-errors (url-retrieve-synchronously url t t 120))))
    (when buffer
      (unwind-protect
          (with-current-buffer buffer
            (buffer-string))
        (kill-buffer buffer)))))

(ert-deftest my/eglot-tests-upstream-wrapped-overlay-bug-open ()
  "Canary: debbugs 67470 about wrapped overlay strings is still open.
If this fails, the bug is closed.  Check whether wrapped inlay hints
still block scrolling.  If not, remove the `visual-line-mode' hook in
the init.el block `eglot' and this test."
  (let ((report (my/eglot-tests--bug-report
                 my/eglot-tests-wrapped-overlay-bug)))
    (unless report
      (ert-skip "No network"))
    (should (string-search "GNU bug report logs" report))
    (should-not (string-search "<strong>Done:</strong>" report))))

;;; eglot-tests.el ends here
