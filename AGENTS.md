# .emacs.d

Emacs configuration (`early-init.el`, `init.el`).

## Running daemon
The user sometimes runs an Emacs daemon.
- If a daemon runs, ask before you load config changes, make temporary changes or run tests in it. Tests in the daemon help diagnose issues the user sees.

### Edit files
Edit files with the direct file edit tools. `global-auto-revert-mode` in `init.el` reverts open buffers in the daemon after the edit. Do not revert them manually.
- Before you edit an existing file, run `emacsclient --eval t`. If it fails, no daemon runs. Edit the file.
- If a daemon runs, check for unsaved changes in the buffer of the file. If the result is `t`, stop and ask the user. This check does not need approval:
  `emacsclient --eval '(let ((buffer (find-buffer-visiting "/abs/path/to/file"))) (and buffer (buffer-modified-p buffer)))'`
- After the edit, ask before you evaluate the change in the daemon.

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

## Batch tests
- To test a change in batch Emacs, write an ERT test file. Do not test with only `--eval` forms.
- Put the file where it stays if the user keeps it:
  - For a feature of `early-init.el` or `init.el`: `test/<feature>-tests.el`.
  - For a package in `lisp/<pkg>/`: `lisp/<pkg>/<pkg>-tests.el`.
  - If a test file for the feature exists, add the tests to it.
- In the Commentary of a new file, write the command that runs it.
- Run it with `emacs -Q --batch ... -f ert-run-tests-batch-and-exit`.
- After the tests run, ask the user with AskUserQuestion whether to keep the tests in the checkout as part of the commit.
  - If the user says yes, keep the file and include it in the commit. Put a comment directly above the code of the new feature. In the comment, give the path of the test file.
  - If the user says no, remove the new file, or remove the added tests from an existing file.

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
