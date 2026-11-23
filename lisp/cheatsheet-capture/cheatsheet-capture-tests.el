;;; cheatsheet-capture-tests.el --- Tests for the key binding capture -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for the Org capture template "k" in init.el and the
;; functions of cheatsheet-capture.el.  The template adds a key binding
;; to cheatsheet.org.
;;
;; The tests read the template from the org block of init.el, so they
;; test the real config without loading all of init.el.  Each test
;; captures into a small cheat sheet in a temporary directory.
;;
;; Run all tests from the repository root:
;;
;;   emacs -Q --batch -L lisp/cheatsheet-capture \
;;         -l lisp/cheatsheet-capture/cheatsheet-capture-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'org-capture)
(require 'dired)
(require 'cheatsheet-capture)

(defconst cheatsheet-capture-tests--init-file
  (let ((test-directory (file-name-directory
                         (or load-file-name buffer-file-name))))
    (expand-file-name "../../init.el" test-directory)))

(defconst cheatsheet-capture-tests--sheet
  "#+TITLE: Test Cheat Sheet

* dired
| Key | Command | Note |
|-----+---------+------|
| ~W~ | copy    |      |

* org
| Key     | Command     | Note |
|---------+-------------+------|
| ~C-c c~ | org-capture |      |
")

;;;; Helpers

(defun cheatsheet-capture-tests--org-form ()
  "Return the (use-package org ...) form of init.el."
  (with-temp-buffer
    (insert-file-contents cheatsheet-capture-tests--init-file)
    (let ((case-fold-search nil))
      (re-search-forward "^(use-package org$")
      (goto-char (match-beginning 0))
      (read (current-buffer)))))

(defun cheatsheet-capture-tests--templates (directory)
  "Return `org-capture-templates' from the org block of init.el.
Compute the cheat sheet path with DIRECTORY as `user-emacs-directory'."
  (let* ((org-form (cheatsheet-capture-tests--org-form))
         (after-custom (cdr (memq :custom org-form)))
         (custom-entries (seq-take-while #'consp after-custom))
         (entry (assq 'org-capture-templates custom-entries))
         (value-form (cadr entry))
         (user-emacs-directory (file-name-as-directory directory)))
    (eval value-form t)))

(defmacro cheatsheet-capture-tests--with-sheet (&rest body)
  "Run BODY with a temporary cheat sheet and the templates of init.el.
BODY can use `directory' and `sheet-file'.  The capture hook
`cheatsheet-capture-remove-extra-newline' is on.  Afterward, kill the
buffer of the cheat sheet without saving it."
  (declare (indent 0) (debug t))
  `(ert-with-temp-directory directory
     (let* ((sheet-file (expand-file-name "cheatsheet.org" directory))
            (org-capture-templates
             (cheatsheet-capture-tests--templates directory))
            (org-capture-prepare-finalize-hook
             '(cheatsheet-capture-remove-extra-newline)))
       (with-temp-file sheet-file
         (insert cheatsheet-capture-tests--sheet))
       (unwind-protect
           (progn ,@body)
         (let ((buffer (find-buffer-visiting sheet-file)))
           (when buffer
             (with-current-buffer buffer
               (set-buffer-modified-p nil))
             (kill-buffer buffer)))))))

(defun cheatsheet-capture-tests--start (section keys)
  "Start capture \"k\".  Answer SECTION and KEYS, and return the prompt count.
The capture buffer is current afterward."
  (let ((prompts 0))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _)
                 (setq prompts (1+ prompts))
                 section))
              ((symbol-function 'read-string)
               (lambda (&rest _)
                 (setq prompts (1+ prompts))
                 ""))
              ((symbol-function 'read-key-sequence)
               (lambda (&rest _)
                 (kbd keys))))
      (org-capture nil "k"))
    prompts))

(defun cheatsheet-capture-tests--text (sheet-file)
  "Return the text of the buffer that visits SHEET-FILE."
  (let ((buffer (find-buffer-visiting sheet-file)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun cheatsheet-capture-tests--section (text heading)
  "Return the part of TEXT from the top-level HEADING to the next one."
  (let* ((heading-line (format "^\\* %s\n" (regexp-quote heading)))
         (start (string-match heading-line text))
         (body-start (match-end 0))
         (end (string-match "^\\* " text body-start)))
    (should start)
    (substring text start end)))

;;;; Tests

(ert-deftest cheatsheet-capture-tests-existing-section ()
  "A row goes into the table of the chosen section."
  (cheatsheet-capture-tests--with-sheet
    (cheatsheet-capture-tests--start "dired" "C-x d")
    (org-capture-finalize)
    (let* ((text (cheatsheet-capture-tests--text sheet-file))
           (dired-section (cheatsheet-capture-tests--section text "dired"))
           (org-section (cheatsheet-capture-tests--section text "org")))
      (should (string-match-p "^| ~W~ +| copy +| +|\n| ~C-x d~ +| dired +| +|$"
                              dired-section))
      (should-not (string-search "C-x d" org-section)))))

(ert-deftest cheatsheet-capture-tests-mode-keymap ()
  "The command comes from the keymaps of the buffer where capture starts."
  (cheatsheet-capture-tests--with-sheet
    (let ((dired-buffer (dired-noselect directory)))
      (unwind-protect
          (with-current-buffer dired-buffer
            (cheatsheet-capture-tests--start "dired" "% m")
            (org-capture-finalize))
	(kill-buffer dired-buffer)))
    (let ((text (cheatsheet-capture-tests--text sheet-file)))
      (should (string-match-p "^| ~% m~ +| dired-mark-files-regexp +|" text)))))

(ert-deftest cheatsheet-capture-tests-new-section ()
  "A new section name adds a heading with a table header at the end."
  (cheatsheet-capture-tests--with-sheet
    (cheatsheet-capture-tests--start "eww" "C-x d")
    (org-capture-finalize)
    (let* ((text (cheatsheet-capture-tests--text sheet-file))
           (eww-section (cheatsheet-capture-tests--section text "eww")))
      (should (string-match-p "\n\n\\* eww\n[^*]*\\'" text))
      (should (string-match-p
               (concat "^| Key +| Command +| Note +|\n"
                       "|-+\\+-+\\+-+|\n"
                       "| ~C-x d~ +| dired +| +|$")
               eww-section)))))

(ert-deftest cheatsheet-capture-tests-vertical-bar ()
  "A | in the key becomes \\vert, so the table keeps three cells."
  (cheatsheet-capture-tests--with-sheet
    (cheatsheet-capture-tests--start "org" "M-|")
    (org-capture-finalize)
    (let* ((text (cheatsheet-capture-tests--text sheet-file))
           (org-section (cheatsheet-capture-tests--section text "org")))
      (should (string-match-p
               "^| ~M-\\\\vert~ +| shell-command-on-region +| +|$"
               org-section)))))

(ert-deftest cheatsheet-capture-tests-no-note-prompt ()
  "Capture asks only for the section.  Point starts in the key cell."
  (cheatsheet-capture-tests--with-sheet
    (let ((prompts (cheatsheet-capture-tests--start "org" "C-x d")))
      (unwind-protect
          (progn
            (should (= prompts 1))
            (should (looking-back "^| " (line-beginning-position)))
            (should (looking-at-p "~C-x d~ | dired |")))
	(org-capture-kill)))))

(ert-deftest cheatsheet-capture-tests-tab-keeps-one-newline ()
  "TAB to the note cell does not add a blank line after the table.
Org 9.8.7 adds one: `org-capture-place-table-line' narrows the capture
buffer without the final newline of the row, and `org-table-align'
adds a newline inside the narrowing.  The hook
`cheatsheet-capture-remove-extra-newline' removes it."
  (cheatsheet-capture-tests--with-sheet
    (cheatsheet-capture-tests--start "dired" "C-x d")
    (org-table-next-field)
    (org-table-next-field)
    (insert "note")
    (org-capture-finalize)
    (let ((text (cheatsheet-capture-tests--text sheet-file)))
      (should (string-match-p "| note +|\n\n\\* org" text)))))

(provide 'cheatsheet-capture-tests)

;;; cheatsheet-capture-tests.el ends here
