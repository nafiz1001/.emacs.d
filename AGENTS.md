# .emacs.d

Emacs configuration (`early-init.el`, `init.el`).

## Running daemon
The user sometimes runs an Emacs daemon.
- If a daemon runs, ask before you load config changes, make temporary changes or run tests in it. Tests in the daemon help diagnose issues the user sees.

### Sandbox
The Bash sandbox blocks the daemon socket and hides the processes outside the sandbox.
- If `emacsclient` fails with `Operation not permitted`, the result is unknown. Do not read it as "no daemon".
- Do not use `pgrep` or `ps` to find the daemon. They show only the processes in the sandbox.
- If the result is unknown and a Terminal panel tool (`run_in_terminal`) is available, run each `emacsclient` command there. That shell runs outside the sandbox. Read the output with `read_terminal`.
- If no Terminal panel tool is available, look for the socket file `$XDG_RUNTIME_DIR/emacs/server`. If it exists, a daemon probably runs. Give the user each `emacsclient` command. Wait for the result before you continue.

### Edit files
Edit files with the direct file edit tools. `global-auto-revert-mode` in `init.el` reverts open buffers in the daemon after the edit. Do not revert them manually.
- Before you edit an existing file, run `emacsclient --eval t`. If it fails with `can't find socket`, no daemon runs. Edit the file. If it fails for another reason, see Sandbox.
- If a daemon runs, check for unsaved changes in the buffer of the file. If the result is `t`, stop and ask the user. This check does not need approval:
  `emacsclient --eval '(let ((buffer (find-buffer-visiting "/abs/path/to/file"))) (and buffer (buffer-modified-p buffer)))'`
- After the edit, ask before you evaluate the change in the daemon.

### Refresh after git
Run git in the shell. Shell git does not update VC, diff-hl or Magit in the daemon.
- After a git write operation, run `emacsclient --eval t`. If it fails with `can't find socket`, no daemon runs. Do nothing more. If it fails for another reason, see Sandbox.
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

### Named functions
- Prefer named functions to anonymous functions. If you put a function in a hook, an advice, a variable, a key binding or a capture template, define it with `defun` and use its name. Reasons: a backtrace shows the name, `debug-on-entry`, `trace-function` and `edebug-defun` work, and evaluating a changed `defun` replaces the old definition. A changed lambda on a hook adds a second copy.
- This rule overrides the rule against single-use abstractions.
- Exception: a short lambda that the called function uses immediately and does not keep. Examples: the function argument of `mapcar`, `seq-filter` or `org-map-entries`.
- Give each new function of the config the prefix `my/`.
- In a `use-package` block, define the function in `:preface`. `:hook`, `:bind` and `:custom` can use the function before `:config` runs.

### Bind arguments
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

### Portability
The config runs on Linux (any distribution), macOS (GUI and terminal) and native Windows. Write each change so that it works on all of them.
- If code needs an external program, test for it with `executable-find`. If the program is missing, keep the Emacs default or skip the feature. Do not signal an error at startup.
- Test `system-type` only if the OS itself is the dependency. Examples: a pty, flatpak, the PATH of macOS GUI Emacs. Do not test for a Linux distribution.
- To skip a package on an OS, wrap the `use-package` form in `when` or `unless`. Do not use `:if`. Reason: use-package installs an `:ensure` package even if `:if` is nil.
- If you use an option that only GNU tools accept, add a fallback. Example: `ls --group-directories-first` fails with BSD `ls` on macOS.
- Do not hard-code Unix paths such as `/tmp` or `/usr/bin`. Use `temporary-file-directory` and `executable-find`. Build file names with `expand-file-name` or `file-name-concat`.
- To run a program, prefer `call-process` or `make-process` with a list of arguments. A shell string for `shell-command` runs in cmd.exe on Windows.
- If a setting uses a path that exists only on one machine, set it only if `file-directory-p` or `file-exists-p` is true.
- To test a branch for another system, stub `executable-find` with `cl-letf` and bind `system-type` with `let` in an ERT test.
