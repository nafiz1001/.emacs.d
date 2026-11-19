;;; ts-indent-guess.el --- Guess indentation from tree-sitter indent rules -*- lexical-binding: t -*-

;; Copyright (C) 2026 Nafiz Islam

;; Author: Nafiz Islam
;;         Claude Opus 5.5 <noreply@anthropic.com>
;; Maintainer: Nafiz Islam
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, languages

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

;; Guess the indentation of a file in a tree-sitter major mode.
;;
;; For each candidate offset, the indent rules of the major mode give
;; the column of each line.  The offset for which most lines are already
;; at that column wins.  Lines that the rules align, such as arguments
;; and continuation lines, get the same column for each candidate, so
;; they do not change the result.
;;
;; Precedence: .editorconfig and file variables, then the guess, then
;; the default offset of the major mode.  Buffers without a tree-sitter
;; parser keep the defaults.
;;
;; The offset variables of each major mode come from the built-in
;; editorconfig mapping (`editorconfig-indent-size-vars').
;;
;; Usage:
;;
;;   (ts-indent-guess-mode 1)
;;
;; The mode line then shows the style and size, for example "Spaces:2".
;; Click it, or type M-x ts-indent-guess-set, to change them.
;;
;; Limitations:
;;
;; Detection calls `treesit--indent-1', an internal function of
;; treesit.el.  Emacs has no public function that returns the
;; (ANCHOR . OFFSET) of a line without a change to the buffer.  An Emacs
;; release can rename or change this function without notice.  If that
;; happens, detection does nothing: each call fails inside
;; `ignore-errors', and each candidate gets 0 matches.  The byte compiler
;; warns if the function no longer exists.  Tested with Emacs 31.1.

;;; Code:

(require 'cl-lib)
(require 'editorconfig)
(require 'seq)
(require 'treesit)

(defgroup ts-indent-guess nil
  "Guess indentation from tree-sitter indent rules."
  :group 'indent
  :prefix "ts-indent-guess-")

(defcustom ts-indent-guess-mode-line t
  "If non-nil, show the indentation style and size in the mode line.
Click the indicator to call `ts-indent-guess-set'."
  :type 'boolean)

(defun ts-indent-guess--offset-vars (size)
  "Return (VAR . SIZE) pairs that set the indent step of this major mode.
Use the same mapping as `.editorconfig' `indent_size'."
  ;; The mapping can return `eval' forms and variables of packages that
  ;; are not installed, for example `py-indent-offset' of python-mode.el.
  (seq-filter (lambda (pair) (and (symbolp (car pair))
                                  (not (eq (car pair) 'eval))
                                  (boundp (car pair))))
              (if (functionp editorconfig-indent-size-vars)
                  (funcall editorconfig-indent-size-vars size)
                (mapcar (lambda (var) (cons var size))
                        editorconfig-indent-size-vars))))

(defun ts-indent-guess-size ()
  "Return the indent step of the current buffer."
  (if indent-tabs-mode
      tab-width
    (or (seq-some (lambda (pair)
                    (let ((value (symbol-value (car pair))))
                      (and (natnump value) value)))
                  (ts-indent-guess--offset-vars 1))
        standard-indent)))

(defun ts-indent-guess-set (style size &optional convert)
  "Indent the current buffer with STYLE (\"Spaces\" or \"Tabs\") of SIZE.
With a prefix argument CONVERT, also convert the leading whitespace."
  (interactive
   (list (completing-read "Indent using: " '("Spaces" "Tabs") nil t nil nil
                          (if indent-tabs-mode "Tabs" "Spaces"))
         (read-number "Size: " (ts-indent-guess-size))
         current-prefix-arg))
  (let ((tabs (equal style "Tabs"))
        (vars (ts-indent-guess--offset-vars size)))
    (setq-local indent-tabs-mode tabs
                tab-width size
                standard-indent size)
    (pcase-dolist (`(,var . ,value) vars)
      (set (make-local-variable var) value))
    (when convert
      ;; tabify.el is not loaded yet, so declare the variable dynamic here.
      (defvar tabify-regexp)
      ;; The default regexp also matches whitespace after code.
      (let ((tabify-regexp "^\t* [ \t]+"))
        (if tabs
            (tabify (point-min) (point-max))
          (untabify (point-min) (point-max)))))
    (force-mode-line-update)
    (message "%s: %d (%s)" style size
             (mapconcat (lambda (pair) (symbol-name (car pair))) vars ", "))))

(defvar ts-indent-guess--mode-line
  '(:eval
    (when (and ts-indent-guess-mode-line
               (derived-mode-p 'prog-mode 'text-mode))
      (propertize
       (format "%s:%d " (if indent-tabs-mode "Tabs" "Spaces")
               (ts-indent-guess-size))
       'mouse-face 'mode-line-highlight
       'help-echo "mouse-1: Set indentation"
       'local-map (make-mode-line-mouse-map
                   'mouse-1 #'ts-indent-guess-set))))
  "Mode-line construct that shows the indentation style and size.")
;; The mode line ignores `:eval' in a variable without this property.
(put 'ts-indent-guess--mode-line 'risky-local-variable t)

(defun ts-indent-guess-detect ()
  "Set the indentation of this tree-sitter buffer from its content.
Try each candidate offset with the indent rules of the major mode, and
keep the offset for which the rules give the existing column on most
lines.  Do nothing if .editorconfig or file variables set the offset."
  (interactive)
  (when (and buffer-file-name (treesit-parser-list))
    (let* ((offset-vars (ts-indent-guess--offset-vars 1))
           (offset-targets (mapcar (lambda (pair)
                                     (let ((var (car pair)))
                                       (indirect-variable var)))
                                   offset-vars))
           (explicit (seq-some (lambda (pair)
                                 (let* ((var (car pair))
                                        (target (indirect-variable var)))
                                   (or (eq target 'indent-tabs-mode)
                                       (memq target offset-targets))))
                               file-local-variables-alist))
           (tab-lines 0)
           (space-lines 0)
           (space-bols nil))
      (when (and offset-vars (not explicit))
        (save-excursion
          (goto-char (point-min))
          (dotimes (_ 2000)
            (unless (or (eobp) (looking-at-p "[ \t]*$"))
              (pcase (char-after)
                (?\t (setq tab-lines (1+ tab-lines)))
                (?\s (setq space-lines (1+ space-lines))
                     (skip-chars-forward " \t")
                     (push (point) space-bols))))
            (forward-line 1)))
        (let ((tab-threshold (* 3 space-lines)))
          (cond
           ((and (> tab-lines 0) (>= tab-lines tab-threshold))
            ;; The tab width is a display choice, so keep the current one.
            (let ((inhibit-message t)
                  (message-log-max nil))
              (ts-indent-guess-set "Tabs" tab-width)))
           (space-bols
            ;; Injected parsers need their ranges before the rules run.
            (treesit-update-ranges)
            (let* ((ordered-bols (nreverse space-bols))
                   (samples (take 500 ordered-bols))
                   (scores
                    (mapcar
                     (lambda (candidate)
                       (let* ((pairs (ts-indent-guess--offset-vars candidate))
                              (vars (mapcar #'car pairs))
                              (values (mapcar #'cdr pairs))
                              (matches 0))
                         ;; The indent rules read the offset variables when
                         ;; they run, so a dynamic binding tests CANDIDATE.
                         (cl-progv vars values
                           (save-excursion
                             (dolist (bol samples)
                               (goto-char bol)
                               ;; `treesit--indent-1' is internal.  See
                               ;; Limitations in the Commentary.  It runs the
                               ;; indent rules for this line and does not
                               ;; change the buffer.
                               (pcase (ignore-errors (treesit--indent-1))
                                 (`(,anchor . ,offset)
                                  (when (and anchor (numberp offset))
                                    (let* ((anchor-column
                                            (save-excursion
                                              (goto-char anchor)
                                              (current-column)))
                                           (column (+ anchor-column offset)))
                                      (when (= column (current-indentation))
                                        (setq matches (1+ matches))))))))))
                         (cons candidate matches)))
                     '(1 2 3 4 8)))
                   (sorted (sort scores :key #'cdr :reverse t))
                   (best (car sorted))
                   (best-size (car best))
                   (best-matches (cdr best))
                   (runner-up (cadr sorted))
                   (runner-up-matches (cdr runner-up)))
              (when (and (> best-matches 0)
                         (> best-matches runner-up-matches))
                (let ((inhibit-message t)
                      (message-log-max nil))
                  (ts-indent-guess-set "Spaces" best-size)))))))))))

;;;###autoload
(define-minor-mode ts-indent-guess-mode
  "Guess the indentation of tree-sitter buffers when files open.
Also show the indentation in the mode line, if
`ts-indent-guess-mode-line' is non-nil."
  :global t
  (if ts-indent-guess-mode
      (progn
        ;; This hook runs after the major mode creates its parsers, and
        ;; after .editorconfig and file variables are applied.
        (add-hook 'hack-local-variables-hook #'ts-indent-guess-detect)
        (add-to-list 'mode-line-misc-info 'ts-indent-guess--mode-line))
    (remove-hook 'hack-local-variables-hook #'ts-indent-guess-detect)
    (setq mode-line-misc-info
          (delq 'ts-indent-guess--mode-line mode-line-misc-info))))

(provide 'ts-indent-guess)
;;; ts-indent-guess.el ends here
