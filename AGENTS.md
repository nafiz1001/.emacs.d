# .emacs.d

Emacs configuration (`early-init.el`, `init.el`).

## Running daemon
The user sometimes runs an Emacs daemon.
- If a daemon runs, ask before you load config changes, make temporary changes or run tests in it. Tests in the daemon help diagnose issues the user sees.

### Edit through the daemon
Edits through the daemon update its open buffers at once, with undo history and save hooks.
- Before you edit a file, run `emacsclient --eval t`. If it fails, no daemon runs. Edit the file directly.
- If a daemon runs, edit existing files through `emacsclient --eval`. Do not use direct file edit tools on them. This edit does not need approval.
- Put the old text and the new text in two files in the scratchpad directory. Do not quote them in the shell command.
- If the buffer has unsaved changes, stop and ask the user.
- If the old text is missing or not unique, stop and fix the old text.
- To create a new file, write it directly.
- After the edit, ask before you evaluate the change in the daemon.

Write this form to `edit.el` in the scratchpad directory. Replace the three paths. Run `emacsclient --eval "$(cat <scratchpad>/edit.el)"`.

```elisp
(let* ((buffer (find-file-noselect "/abs/path/to/file"))
       (old (with-temp-buffer
              (insert-file-contents "<scratchpad>/old.txt")
              (buffer-string)))
       (new (with-temp-buffer
              (insert-file-contents "<scratchpad>/new.txt")
              (buffer-string))))
  (with-current-buffer buffer
    (when (buffer-modified-p)
      (user-error "Buffer has unsaved changes"))
    (save-excursion
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (unless (search-forward old nil t)
          (user-error "Old text not found"))
        (when (save-excursion (search-forward old nil t))
          (user-error "Old text is not unique"))
        (replace-match new t t)))
    (save-buffer)))
```

### Refresh after git
Run git in the shell. Shell git does not update VC, diff-hl or Magit in the daemon.
- After a git write operation, run `emacsclient --eval t`. If it fails, no daemon runs. Do nothing more.
- If a daemon runs, run `refresh.el`. Examples of write operations: `commit`, `merge`, `switch`, `stash`, `reset`, `pull`.
- This refresh does not need approval.

Write this form to `refresh.el` in the scratchpad directory. Replace the repo path. Run `emacsclient --eval "$(cat <scratchpad>/refresh.el)"`.

```elisp
(let ((root "/abs/path/to/repo/"))
  ;; file-in-directory-p expands the default-directory of every buffer.
  ;; For a stale TRAMP buffer, TRAMP opens a new connection.  With
  ;; non-essential, TRAMP uses only connections that are already open.
  (let ((non-essential t))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (file-in-directory-p default-directory root)
          (cond
           ;; Revert now. global-auto-revert-mode reverts later, so the
           ;; Magit refresh below sees stale buffers.
           ((and buffer-file-name
                 (not (buffer-modified-p))
                 (not (verify-visited-file-modtime)))
            (revert-buffer t t t))
           (buffer-file-name
            (vc-refresh-state)
            (when (bound-and-true-p diff-hl-mode)
              (diff-hl-update)))
           ((derived-mode-p 'vc-dir-mode)
            (vc-dir-refresh)))))))
  (let ((default-directory root))
    (when (fboundp 'magit-refresh-all)
      (magit-refresh-all))))
```

## Emacs Lisp
- If an argument of a function call is a function call, bind that argument to a name with `let` or `let*`. Then pass the name.
- These forms are exempt. Their arguments can be function calls:
  - Special forms and macros that control evaluation. Examples: `and`, `or`, `not`, `if`, `when`, `unless`, `cond`, `pcase`, `progn`, `while`, `dolist`, `condition-case`, `save-excursion`, `with-*`.
  - Assignment and definition forms. Examples: `setq`, `setopt`, `defvar`, `defcustom`, `add-to-list`.
  - `lambda` forms.
- A call with no arguments, such as `(point)` or `(buffer-name)`, does not need a binding.
- Give each binding a name that a reader understands before they read its value.
- If you cannot find such a name, change the nesting. Put more calls in the value, or split the value into more bindings.
- If a binding uses an earlier binding, use `let*`.
- Apply this rule only to code that you add or change.

Example:

```elisp
;; Do not write:
(message "%s" (string-trim (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))

;; Write:
(let* ((line (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position)))
       (trimmed (string-trim line)))
  (message "%s" trimmed))
```
