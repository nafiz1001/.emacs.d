;;; ts-indent-guess-tests.el --- Tests for ts-indent-guess -*- lexical-binding: t -*-

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

;; ERT tests for ts-indent-guess.el.
;;
;; Run all tests from the repository root:
;;
;;   emacs -Q --batch -L lisp/ts-indent-guess \
;;         -l lisp/ts-indent-guess/ts-indent-guess-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or, in a running Emacs, load this file and type M-x ert RET t RET.
;; The selector t runs all loaded tests.  A string selector, such as
;; "ts-indent-guess-test-detect", runs only the tests whose name matches.
;;
;; Tests that need a tree-sitter grammar are skipped if the grammar is
;; not installed.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'ts-indent-guess)

;; These major modes load only when a test calls them.  Without these
;; declarations, `let' in this lexical-binding file makes a lexical
;; variable with the same name, and the major mode never sees the value.
(defvar json-ts-indent-offset)
(defvar rust-ts-indent-offset)

;;;; Helpers

(defmacro ts-indent-guess-tests--visiting (files &rest body)
  "Write FILES to a temporary directory, visit the last one, and run BODY.
FILES is a list of (NAME . CONTENT).  BODY runs in the buffer of the
last file, with `ts-indent-guess-mode' on.  Afterward, kill the buffer,
delete the directory, and restore the mode."
  (declare (indent 1) (debug t))
  `(ert-with-temp-directory dir
     (let ((was-on ts-indent-guess-mode)
           ;; Under emacs -Q, `auto-mode-alist' does not always map these
           ;; extensions to the tree-sitter modes.  Bind it so each test
           ;; gets the same major mode on each Emacs setup.
           (auto-mode-alist '(("\\.rs\\'" . rust-ts-mode)
                              ("\\.json\\'" . json-ts-mode)
                              ("\\.py\\'" . python-ts-mode)
                              ("\\.el\\'" . emacs-lisp-mode)))
           (path nil))
       (dolist (file ,files)
         (let* ((name (car file))
                (content (cdr file)))
           (setq path (expand-file-name name dir))
           (with-temp-file path
             (insert content))))
       (ts-indent-guess-mode 1)
       ;; `unwind-protect' runs the cleanup even when a `should' fails.
       ;; Without it, a failed test leaves a buffer or a global mode
       ;; behind, and later tests fail for the wrong reason.
       (let ((buffer (find-file-noselect path)))
         (unwind-protect
             (with-current-buffer buffer
               ,@body)
           (kill-buffer buffer)
           (unless was-on
             (ts-indent-guess-mode -1)))))))

;;;; Offset variables

(ert-deftest ts-indent-guess-test-offset-vars ()
  "The editorconfig mapping gives the offset variable of the major mode."
  ;; `skip-unless' marks the test as skipped, not failed, when the
  ;; condition is nil.  A missing grammar is a setup issue, not a bug.
  (skip-unless (treesit-language-available-p 'rust))
  ;; A test that needs no file can use a temporary buffer.
  (with-temp-buffer
    (rust-ts-mode)
    (let ((pairs (ts-indent-guess--offset-vars 3)))
      ;; `should' fails the test if its form returns nil.  ERT then shows
      ;; the form and the values in it, so prefer a simple comparison
      ;; over a complex one.
      (should (member '(rust-ts-indent-offset . 3) pairs)))))

;;;; Detection

(ert-deftest ts-indent-guess-test-detect-two-spaces ()
  "A Rust file with 2-space indentation gets offset 2.
The aligned `vec!' argument does not change the result."
  (skip-unless (treesit-language-available-p 'rust))
  (ts-indent-guess-tests--visiting
      '(("main.rs" . "\
fn main() {
  let v = vec![1,
               2];
  for x in v {
    if x > 1 {
      println!(\"{}\", x);
    }
  }
}
"))
    (should (= rust-ts-indent-offset 2))
    ;; `should-not' is the reverse of `should'.
    (should-not indent-tabs-mode)))

(ert-deftest ts-indent-guess-test-detect-four-spaces ()
  "A Rust file with 4-space indentation gets offset 4."
  (skip-unless (treesit-language-available-p 'rust))
  ;; The default offset of `rust-ts-mode' is 4 already, so with that
  ;; default this test passes even if detection does nothing.  Start from
  ;; 2, so only detection can make the result 4.
  (let ((rust-ts-indent-offset 2))
    (ts-indent-guess-tests--visiting
        '(("main.rs" . "\
fn main() {
    for x in v {
        if x > 1 {
            f(x);
        }
    }
}
"))
      (should (= rust-ts-indent-offset 4)))))

(ert-deftest ts-indent-guess-test-detect-one-space ()
  "A JSON file with 1-space indentation gets offset 1."
  (skip-unless (treesit-language-available-p 'json))
  ;; Some Emacs versions do not map `json-ts-mode' in editorconfig.  A
  ;; `let' binding of a global variable changes it for this test only,
  ;; so other tests still see the original value.
  (let ((editorconfig-indentation-alist
         (cons '(json-ts-mode json-ts-indent-offset)
               editorconfig-indentation-alist)))
    (ts-indent-guess-tests--visiting
        '(("data.json" . "\
{
 \"a\": {
  \"b\": [
   1,
   2
  ]
 },
 \"c\": 3
}
"))
      (should (= json-ts-indent-offset 1)))))

(ert-deftest ts-indent-guess-test-detect-tabs ()
  "A Python file with tab indentation gets `indent-tabs-mode'."
  (skip-unless (treesit-language-available-p 'python))
  (ts-indent-guess-tests--visiting
      '(("main.py" . "\
def f():
\tif x:
\t\treturn 1
\tfor y in z:
\t\tpass
"))
    (should indent-tabs-mode)))

(ert-deftest ts-indent-guess-test-detect-no-evidence ()
  "A file with no indented lines keeps the default offset."
  (skip-unless (treesit-language-available-p 'rust))
  (ts-indent-guess-tests--visiting
      '(("main.rs" . "fn main() {}\n"))
    ;; Test the absence of a change: no buffer-local value was made.
    (should-not (local-variable-p 'rust-ts-indent-offset))))

(ert-deftest ts-indent-guess-test-detect-skips-non-tree-sitter ()
  "A buffer without a tree-sitter parser keeps the defaults."
  (ts-indent-guess-tests--visiting
      '(("init.el" . "\
(defun f ()
  (let ((a 1))
    a))
"))
    (should-not (local-variable-p 'lisp-indent-offset))
    (should-not (local-variable-p 'indent-tabs-mode))))

;;;; Precedence

(ert-deftest ts-indent-guess-test-editorconfig-wins ()
  "Settings from .editorconfig win over detection."
  (skip-unless (treesit-language-available-p 'rust))
  (let ((editorconfig-was-on editorconfig-mode))
    (editorconfig-mode 1)
    (unwind-protect
        (ts-indent-guess-tests--visiting
            '((".editorconfig" . "\
root = true
[*]
indent_style = tab
indent_size = 8
")
              ("main.rs" . "\
fn main() {
  if true {
    a();
  }
  if false {
    b();
  }
}
"))
          (should indent-tabs-mode)
          (should (= rust-ts-indent-offset 8)))
      (unless editorconfig-was-on
        (editorconfig-mode -1)))))

(ert-deftest ts-indent-guess-test-file-variable-wins ()
  "A file variable wins over detection."
  (skip-unless (treesit-language-available-p 'rust))
  (ts-indent-guess-tests--visiting
      '(("main.rs" . "\
// -*- rust-ts-indent-offset: 8 -*-
fn main() {
  if true {
    a();
  }
}
"))
    (should (= rust-ts-indent-offset 8))))

;;;; Setting the indentation

(ert-deftest ts-indent-guess-test-set-converts ()
  "`ts-indent-guess-set' with CONVERT changes leading spaces to tabs."
  (skip-unless (treesit-language-available-p 'rust))
  (with-temp-buffer
    (rust-ts-mode)
    (insert "fn f() {\n    a();\n}\n")
    ;; Bind `inhibit-message' so the command does not print to the test
    ;; output.
    (let ((inhibit-message t))
      (ts-indent-guess-set "Tabs" 4 t))
    (should indent-tabs-mode)
    (should (= rust-ts-indent-offset 4))
    (should (equal (buffer-string) "fn f() {\n\ta();\n}\n"))))

;;;; Mode and mode line

(ert-deftest ts-indent-guess-test-mode-toggle ()
  "The mode adds the hook and the mode-line entry, and removes them."
  (let ((was-on ts-indent-guess-mode))
    (unwind-protect
        (progn
          (ts-indent-guess-mode 1)
          (should (memq #'ts-indent-guess-detect hack-local-variables-hook))
          (should (memq 'ts-indent-guess--mode-line mode-line-misc-info))
          (ts-indent-guess-mode -1)
          (should-not (memq #'ts-indent-guess-detect
                            hack-local-variables-hook))
          (should-not (memq 'ts-indent-guess--mode-line
                            mode-line-misc-info)))
      (ts-indent-guess-mode (if was-on 1 -1)))))

(ert-deftest ts-indent-guess-test-mode-line-option ()
  "The mode-line text follows `ts-indent-guess-mode-line'."
  (skip-unless (treesit-language-available-p 'rust))
  ;; `format-mode-line' returns "" in batch mode, so evaluate the
  ;; `:eval' form of the construct directly.
  (let ((form (cadr ts-indent-guess--mode-line)))
    (with-temp-buffer
      (rust-ts-mode)
      (let ((ts-indent-guess-mode-line t))
        (should (equal (eval form t) "Spaces:4 ")))
      (let ((ts-indent-guess-mode-line nil))
        (should-not (eval form t))))))

;;; ts-indent-guess-tests.el ends here
