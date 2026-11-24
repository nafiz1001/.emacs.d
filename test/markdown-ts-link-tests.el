;;; markdown-ts-link-tests.el --- Tests for markdown-ts-mode file links -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for the advice on `markdown-ts--make-link-button' in
;; init.el.  A link such as [x](other.md#heading) must open other.md
;; and move point to the heading.  A percent-encoded path such as
;; my%20file.md must open "my file.md".
;;
;; Run:
;;   emacs -Q --batch -l test/markdown-ts-link-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'markdown-ts-mode)

(defconst my/markdown-ts-link-tests-init
  (let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
         (root (file-name-directory (directory-file-name test-dir))))
    (expand-file-name "init.el" root)))

;; Load only the markdown-ts-mode block of init.el, not the whole
;; config.  Evaluate its :preface and :config forms directly.
(with-temp-buffer
  (insert-file-contents my/markdown-ts-link-tests-init)
  (search-forward "(use-package markdown-ts-mode")
  (goto-char (match-beginning 0))
  (let* ((form (read (current-buffer)))
         (body (cdr (cdr form)))
         (keyword nil))
    (dolist (item body)
      (if (keywordp item)
          (setq keyword item)
        (when (memq keyword '(:preface :config))
          (eval item t))))))

;; The :mode keyword of the block, so that find-file opens the target
;; in markdown-ts-mode.
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))

(defun my/markdown-ts-link-tests--push-link (directory text)
  "Insert TEXT into a markdown-ts-mode buffer in DIRECTORY and push its link.
Do not use `with-current-buffer': it restores the source buffer, and
the test must see the buffer that the link opened."
  (set-buffer (get-buffer-create "source.md"))
  (setq default-directory directory)
  (insert text)
  (markdown-ts-mode)
  (font-lock-ensure)
  (goto-char (point-min))
  (search-forward "[")
  (push-button (point)))

(defun my/markdown-ts-link-tests--cleanup (directory)
  "Kill the test buffers and delete DIRECTORY."
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when (or (equal (buffer-name buffer) "source.md")
                (and file (file-in-directory-p file directory)))
        (kill-buffer buffer))))
  (delete-directory directory t))

(ert-deftest my/markdown-ts-link-tests-file-fragment ()
  (skip-unless (treesit-ready-p 'markdown t))
  (let* ((directory (file-name-as-directory (make-temp-file "mdts" t)))
         (target (expand-file-name "sub/other.md" directory)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory target))
          (with-temp-file target
            (insert "# Top\n\ntext\n\n## FMS_ENV values\n\nmore\n"))
          (my/markdown-ts-link-tests--push-link
           directory "[go](sub/other.md#fms_env-values)\n")
          (should (equal buffer-file-name target))
          (should (looking-at-p "## FMS_ENV values")))
      (my/markdown-ts-link-tests--cleanup directory))))

(ert-deftest my/markdown-ts-link-tests-plain-file ()
  (skip-unless (treesit-ready-p 'markdown t))
  (let* ((directory (file-name-as-directory (make-temp-file "mdts" t)))
         (target (expand-file-name "other.md" directory)))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert "# Top\n"))
          (my/markdown-ts-link-tests--push-link directory "[go](other.md)\n")
          (should (equal buffer-file-name target)))
      (my/markdown-ts-link-tests--cleanup directory))))

(ert-deftest my/markdown-ts-link-tests-encoded-file ()
  (skip-unless (treesit-ready-p 'markdown t))
  (let* ((directory (file-name-as-directory (make-temp-file "mdts" t)))
         (target (expand-file-name "my filé.md" directory)))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert "# Top\n"))
          (my/markdown-ts-link-tests--push-link
           directory "[go](my%20fil%C3%A9.md)\n")
          (should (equal buffer-file-name target)))
      (my/markdown-ts-link-tests--cleanup directory))))

(ert-deftest my/markdown-ts-link-tests-encoded-file-fragment ()
  (skip-unless (treesit-ready-p 'markdown t))
  (let* ((directory (file-name-as-directory (make-temp-file "mdts" t)))
         (target (expand-file-name "my file.md" directory)))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert "# Top\n\ntext\n\n## Second part\n"))
          (my/markdown-ts-link-tests--push-link
           directory "[go](my%20file.md#second-part)\n")
          (should (equal buffer-file-name target))
          (should (looking-at-p "## Second part")))
      (my/markdown-ts-link-tests--cleanup directory))))

;; An empty fragment opens the file and does not signal an error.
(ert-deftest my/markdown-ts-link-tests-empty-fragment ()
  (skip-unless (treesit-ready-p 'markdown t))
  (let* ((directory (file-name-as-directory (make-temp-file "mdts" t)))
         (target (expand-file-name "other.md" directory)))
    (unwind-protect
        (progn
          (with-temp-file target
            (insert "# Top\n"))
          (my/markdown-ts-link-tests--push-link directory "[go](other.md#)\n")
          (should (equal buffer-file-name target)))
      (my/markdown-ts-link-tests--cleanup directory))))

(ert-deftest my/markdown-ts-link-tests-upstream-file-link ()
  "Canary: markdown-ts-mode gives a relative URL to `find-file' unchanged.
If this fails, upstream fixed the bug.  Remove the advice
`my/markdown-ts-fix-file-link' in the init.el block `markdown-ts-mode',
its functions, the other tests of this file and this test."
  (skip-unless (treesit-ready-p 'markdown t))
  (let ((directory (file-name-as-directory (make-temp-file "mdts" t))))
    (advice-remove 'markdown-ts--make-link-button
                   #'my/markdown-ts-fix-file-link)
    (unwind-protect
        (progn
          (my/markdown-ts-link-tests--push-link
           directory "[go](other.md#top)\n")
          (should (string-suffix-p "/other.md#top" buffer-file-name))
          (kill-buffer "source.md")
          (my/markdown-ts-link-tests--push-link
           directory "[go](my%20file.md)\n")
          (should (string-suffix-p "/my%20file.md" buffer-file-name)))
      (advice-add 'markdown-ts--make-link-button :after
                  #'my/markdown-ts-fix-file-link)
      (my/markdown-ts-link-tests--cleanup directory))))

;;; markdown-ts-link-tests.el ends here
