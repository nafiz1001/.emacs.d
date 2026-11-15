# .emacs.d

Emacs configuration (`early-init.el`, `init.el`).

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
