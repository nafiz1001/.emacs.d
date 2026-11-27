;;; freezeman-live-tests.el --- Live tests of freezeman.el with podman -*- lexical-binding: t -*-

;;; Commentary:

;; These tests run the real commands in the `dev' container of a
;; Freezeman checkout.  They need podman-compose, the checkout in
;; FREEZEMAN_ROOT, CONTAINER_FMS_ROOT in oci/.env set to that path, and
;; the language servers of oci/compose.local.yaml.  They start
;; runserver on port 8000.  Run from the root of the Emacs
;; configuration, outside any sandbox:
;;
;;   FREEZEMAN_ROOT=/path/to/freezeman emacs -Q --batch -L lisp/freezeman -l lisp/freezeman/freezeman-live-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'freezeman)
(require 'python)
(require 'eglot)

(defvar freezeman-live-tests--root (getenv "FREEZEMAN_ROOT")
  "Root of the Freezeman checkout under test.")

(defun freezeman-live-tests--wait-for (buffer regexp seconds)
  "Wait up to SECONDS for REGEXP in BUFFER.  Return non-nil if found."
  (with-timeout (seconds nil)
    (let ((found nil))
      (while (not found)
        (accept-process-output nil 0.5)
        (setq found (with-current-buffer buffer
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward regexp nil t)))))
      found)))

(defun freezeman-live-tests--kill (buffer)
  "Kill BUFFER and its process with no question."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(ert-deftest freezeman-live-tests-backend-restart ()
  "runserver starts, and starts again after its buffer is killed.
The second start fails with \"port is already in use\" if killing
the buffer leaves runserver alive in the container."
  (skip-unless (and freezeman-live-tests--root
                    (executable-find "podman-compose")))
  (let ((freezeman-project-root freezeman-live-tests--root)
        (display-buffer-alist '((".*" display-buffer-no-window))))
    (dotimes (_ 2)
      (freezeman-backend)
      (let ((buffer (get-buffer "python manage.py runserver")))
        (should (freezeman-live-tests--wait-for
                 buffer "Starting development server\\|port is already in use"
                 120))
        (should-not (with-current-buffer buffer
                      (string-match-p "port is already in use"
                                      (buffer-string))))
        (freezeman-live-tests--kill buffer)
        ;; Let the container process see the hangup.
        (sleep-for 3)))))

(ert-deftest freezeman-live-tests-shell ()
  "The Django shell runs single-line and multi-line code.
python.el sends multi-line code through a temporary file."
  (skip-unless (and freezeman-live-tests--root
                    (executable-find "podman-compose")))
  (let ((freezeman-project-root freezeman-live-tests--root)
        (display-buffer-alist '((".*" display-buffer-no-window))))
    (freezeman-shell)
    (let* ((process (python-shell-get-process-or-error))
           (buffer (process-buffer process)))
      (unwind-protect
          (progn
            (should (freezeman-live-tests--wait-for buffer "^>>> " 120))
            (should (equal (python-shell-send-string-no-output
                            "print(1 + 1)" process)
                           "2"))
            (should (equal (python-shell-send-string-no-output
                            "a = 40\nb = 2\nprint(a + b)" process)
                           "42"))
            ;; The setup code at the first prompt goes through a
            ;; temporary file, which the container must see.
            (should-not (with-current-buffer buffer
                          (string-match-p "No such file\\|Traceback"
                                          (buffer-string))))
            ;; No warning from prompt detection.
            (should-not (get-buffer "*Warnings*")))
        (freezeman-live-tests--kill buffer)))))

(defun freezeman-live-tests--eglot (file mode)
  "Open FILE of the checkout in MODE, start Eglot, return the server."
  (let* ((path (expand-file-name file freezeman-live-tests--root))
         (buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (funcall mode)
      (freezeman-eglot-setup)
      (should (local-variable-p 'eglot-server-programs))
      (let ((eglot-sync-connect 60)
            (contact (eglot--guess-contact)))
        (apply #'eglot contact))
      (eglot-current-server))))

(ert-deftest freezeman-live-tests-eglot ()
  "basedpyright and typescript-language-server start in `dev'."
  (skip-unless (and freezeman-live-tests--root
                    (executable-find "podman-compose")))
  (let ((freezeman-project-root freezeman-live-tests--root))
    (dolist (case '(("backend/fms/settings.py" python-mode :hoverProvider)
                    ("frontend/webpack.config.js" js-mode :hoverProvider)))
      (let ((server (freezeman-live-tests--eglot (nth 0 case) (nth 1 case))))
        (unwind-protect
            (progn
              (should server)
              (should (process-live-p (jsonrpc--process server)))
              (with-current-buffer (car (eglot--managed-buffers server))
                (should (eglot-server-capable (nth 2 case)))))
          (when server
            (eglot-shutdown server)))))))

;;; freezeman-live-tests.el ends here
