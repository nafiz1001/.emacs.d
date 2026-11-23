;;; cheatsheet-capture.el --- Org capture of key bindings into a cheat sheet -*- lexical-binding: t -*-

;; Author: Nafiz Islam
;;         Claude Opus 5.5 <noreply@anthropic.com>

;;; Commentary:

;; Functions for the Org capture template "k" in init.el.  The template
;; adds a row with a key binding and its command to a table in
;; cheatsheet.org.  The template itself stays in init.el.

;;; Code:

(require 'org)
(require 'org-capture)

;; lisp/cheatsheet-capture/cheatsheet-capture-tests.el tests these
;; functions through template "k" of init.el.

(defun cheatsheet-capture-goto-section ()
  "Prompt for a top-level section and move point to it.
If the section does not exist, add it at the end with a table header.
Template \"k\" uses this as the function of its `file+function' target."
  ;; table-line adds the row to the first table under this heading.
  (let* ((sections (org-map-entries
                    (lambda () (org-get-heading t t t t))
                    "LEVEL=1"))
         (section (completing-read "Section: " sections))
         (heading (org-find-exact-headline-in-buffer section)))
    (if heading
        (goto-char heading)
      ;; A new section needs the table header too. Without a
      ;; table, table-line creates one with an empty header.
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert "\n")
      (let ((new-heading (point)))
        (insert "* " section "\n"
                "| Key | Command | Note |\n"
                "|-----+---------+------|\n")
        (goto-char new-heading)))))

(defun cheatsheet-capture-row ()
  "Read a key sequence and return a table row for it.
Template \"k\" uses this as its template function."
  ;; org-capture calls this in the buffer where capture started, so
  ;; key-binding sees the keymaps of that buffer's modes.
  (let* ((keys (read-key-sequence "Key: "))
         (command (key-binding keys))
         (description (key-description keys))
         ;; A bare | ends the table cell.
         (cell (string-replace "|" "\\vert" description)))
    ;; %? leaves point at the start of the key cell. TAB moves to
    ;; the next cell. C-c C-c saves the row with an empty note.
    (format "| %%?~%s~ | %s |  |" cell command)))

(defun cheatsheet-capture-remove-extra-newline ()
  "Remove the extra final newline of a table-line capture buffer.
Use this in `org-capture-prepare-finalize-hook'."
  ;; Org bug (9.8.7): a table-line capture buffer is narrowed
  ;; without the newline that ends the row.  When org-table-align
  ;; runs in it, such as on TAB, it adds a newline inside the
  ;; narrowing, and the file gets a blank line after the table.
  ;; This hook runs while the buffer is still narrowed, so a
  ;; final newline there is always the extra one.
  (let ((type (org-capture-get :type 'local))
        (last-char (char-before (point-max))))
    (when (and (eq type 'table-line)
               (eq last-char ?\n))
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))))

(provide 'cheatsheet-capture)

;;; cheatsheet-capture.el ends here
