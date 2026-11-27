;;; freezeman-tests.el --- Tests for freezeman.el -*- lexical-binding: t -*-

;;; Commentary:

;; Run from the root of the Emacs configuration:
;;
;;   emacs -Q --batch -L lisp/freezeman -l lisp/freezeman/freezeman-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'freezeman)

;; Without this, `let' binds the variable lexically in the tests, and
;; freezeman.el does not see the value.
(defvar eglot-server-programs)

(defconst freezeman-tests--running-dev
  "exited,com.docker.compose.service=db,\nrunning,a=1,com.docker.compose.service=dev,b=2,\n"
  "Output of podman-compose ps when `dev' runs.")

(defvar freezeman-tests--ps-output freezeman-tests--running-dev
  "Output of the stubbed \"podman-compose ps\".")

(defvar freezeman-tests--up-status 0
  "Exit status of the stubbed \"podman-compose up\".")

(defmacro freezeman-tests--with-checkout (marker &rest body)
  "Run BODY in a temporary git repo with a backend settings file.
If MARKER is non-nil, the settings file contains the Freezeman marker.
Bind `root' to the directory name of the repo."
  (declare (indent 1))
  `(let* ((root (make-temp-file "freezeman-tests-" t "/"))
          (settings-dir (file-name-concat root "backend" "fms")))
     (unwind-protect
         (let ((default-directory root))
           (call-process "git" nil nil nil "init" "--quiet")
           (make-directory settings-dir t)
           (let ((settings (file-name-concat settings-dir "settings.py")))
             (with-temp-file settings
               (insert "INSTALLED_APPS = [\n")
               (when ,marker
                 (insert "    'fms_core.apps.FmsCoreConfig',\n"))
               (insert "]\n")))
           ,@body)
       (delete-directory root t))))

(defmacro freezeman-tests--with-stubs (&rest body)
  "Run BODY with the external programs stubbed.
`async-shell-command', `run-python' and `call-process' only record
their calls.  `executable-find' finds every program.  The stubbed
`call-process' prints `freezeman-tests--ps-output' for \"ps\" and
returns `freezeman-tests--up-status' for \"up\".
Bind `calls' to the list of (FUNCTION . ARGS) in call order, with
`default-directory' at the call as the last element of ARGS for
`async-shell-command' and `run-python'."
  `(let ((calls nil))
     (cl-letf (((symbol-function 'executable-find) #'identity)
               ((symbol-function 'call-process)
                (lambda (program &optional _infile destination _display
                                 &rest args)
                  (push (append (list 'call-process program) args) calls)
                  (cond
                   ((member "ps" args)
                    (when destination
                      (insert freezeman-tests--ps-output))
                    0)
                   ((member "up" args) freezeman-tests--up-status)
                   (t 0))))
               ((symbol-function 'async-shell-command)
                (lambda (&rest args)
                  (push (append (list 'async-shell-command) args
                                (list default-directory))
                        calls)))
               ((symbol-function 'run-python)
                (lambda (&rest args)
                  (push (append (list 'run-python) args
                                (list default-directory))
                        calls)
                  ;; run-python returns the process of its buffer.
                  (make-pipe-process
                   :name "freezeman-tests"
                   :buffer (generate-new-buffer "*freezeman-tests-python*")
                   :noquery t))))
       ,@body
       (setq calls (nreverse calls)))
     calls))

(defun freezeman-tests--calls-of (function calls)
  "Return the calls of FUNCTION in CALLS."
  (seq-filter (lambda (call) (eq (car call) function)) calls))

(ert-deftest freezeman-tests-no-project ()
  (let ((freezeman-project-root nil)
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'project-current) #'ignore))
      (dolist (command '(freezeman-backend freezeman-frontend freezeman-shell))
        (should-error (freezeman-tests--with-stubs (funcall command))
                      :type 'user-error)))))

(ert-deftest freezeman-tests-project-with-marker ()
  (freezeman-tests--with-checkout t
    (let ((freezeman-project-root nil)
          (expected (file-name-as-directory (expand-file-name root))))
      (should (equal (freezeman--root) expected))
      (let ((default-directory (file-name-concat root "backend" "fms/")))
        (should (equal (freezeman--root) expected))))))

(ert-deftest freezeman-tests-project-without-marker ()
  (freezeman-tests--with-checkout nil
    (let ((freezeman-project-root nil))
      (should-not (freezeman--find-root))
      (should-error (freezeman--root) :type 'user-error))))

(ert-deftest freezeman-tests-option-wins ()
  (freezeman-tests--with-checkout t
    (let ((freezeman-project-root "/srv/freezeman"))
      (should (equal (freezeman--root) "/srv/freezeman/")))))

(ert-deftest freezeman-tests-compose-args ()
  "compose.local.yaml is added only if it exists."
  (freezeman-tests--with-checkout t
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (oci (file-name-concat root "oci"))
           (base (list "-f" (file-name-concat oci "compose.yaml")
                       "-f" (file-name-concat oci "compose.dev.yaml")))
           (local (file-name-concat oci "compose.local.yaml")))
      (should (equal (freezeman--compose-args root) base))
      (make-directory oci t)
      (with-temp-file local)
      (should (equal (freezeman--compose-args root)
                     (append base (list "-f" local)))))))

(ert-deftest freezeman-tests-backend-command ()
  (let* ((system-type 'gnu/linux)
         (freezeman-project-root "/srv/freezeman")
         (calls (freezeman-tests--with-stubs (freezeman-backend))))
    (should (equal (car (freezeman-tests--calls-of 'async-shell-command calls))
                   '(async-shell-command
                     "podman-compose -f /srv/freezeman/oci/compose.yaml -f /srv/freezeman/oci/compose.dev.yaml exec dev python backend/manage.py runserver 0.0.0.0\\:8000"
                     "python manage.py runserver"
                     "/srv/freezeman/")))))

(ert-deftest freezeman-tests-backend-command-windows ()
  "The space in the root tests the quoting for cmd.exe.
`system-type' is bound inside the stubs: stubbing the primitive
`call-process' compiles a native trampoline, which fails on a fake
`windows-nt'."
  (cl-letf (((symbol-function 'w32-shell-dos-semantics) (lambda () t)))
    (let* ((freezeman-project-root "/srv/free man")
           (calls (freezeman-tests--with-stubs
                   (let ((system-type 'windows-nt))
                     (freezeman-backend))))
           (call (car (freezeman-tests--calls-of 'async-shell-command calls))))
      (should (equal
               (nth 1 call)
               "\"podman-compose\" \"-f\" \"/srv/free man/oci/compose.yaml\" \"-f\" \"/srv/free man/oci/compose.dev.yaml\" \"exec\" \"dev\" \"python\" \"backend/manage.py\" \"runserver\" \"0.0.0.0:8000\"")))))

(ert-deftest freezeman-tests-frontend-command ()
  (let* ((system-type 'gnu/linux)
         (freezeman-project-root "/srv/freezeman")
         (calls (freezeman-tests--with-stubs (freezeman-frontend))))
    (should (equal (car (freezeman-tests--calls-of 'async-shell-command calls))
                   '(async-shell-command
                     "podman-compose -f /srv/freezeman/oci/compose.yaml -f /srv/freezeman/oci/compose.dev.yaml exec dev npm --prefix frontend start -- --host 0.0.0.0"
                     "npm start"
                     "/srv/freezeman/")))))

(ert-deftest freezeman-tests-shell-command ()
  "The shell command, and the temporary files in the checkout."
  (let* ((temp-root (make-temp-file "freezeman-tests-" t))
         (temp-directory (file-name-concat temp-root "emacs-tmp/")))
    (unwind-protect
        (let* ((system-type 'gnu/linux)
               (freezeman-project-root "/srv/freezeman")
               (freezeman-shell-temp-directory temp-directory)
               (calls (freezeman-tests--with-stubs (freezeman-shell)))
               (buffer (get-buffer "*freezeman-tests-python*")))
          (unwind-protect
              (progn
                (should (equal (car (freezeman-tests--calls-of 'run-python calls))
                               '(run-python
                                 "podman-compose -f /srv/freezeman/oci/compose.yaml -f /srv/freezeman/oci/compose.dev.yaml exec dev sh -c \"stty -echo && exec python backend/manage.py shell\""
                                 nil t "/srv/freezeman/")))
                (should (file-directory-p temp-directory))
                (should (equal (buffer-local-value 'temporary-file-directory
                                                   buffer)
                               temp-directory)))
            (let ((kill-buffer-query-functions nil))
              (kill-buffer buffer))))
      (delete-directory temp-root t))))

(ert-deftest freezeman-tests-kill-old-buffer ()
  (let ((freezeman-project-root "/srv/freezeman"))
    (dolist (case '((freezeman-backend . "python manage.py runserver")
                    (freezeman-frontend . "npm start")))
      (let ((old (get-buffer-create (cdr case))))
        (freezeman-tests--with-stubs (funcall (car case)))
        (should-not (buffer-live-p old)))
      ;; No old buffer: the command must not signal an error.
      (freezeman-tests--with-stubs (funcall (car case))))))

(ert-deftest freezeman-tests-dev-running ()
  "If `dev' runs, the commands do not run \"up\"."
  (let* ((freezeman-project-root "/srv/freezeman")
         (calls (freezeman-tests--with-stubs (freezeman-backend)))
         (processes (freezeman-tests--calls-of 'call-process calls)))
    (should (= (length processes) 1))
    (should (member "ps" (car processes)))))

(ert-deftest freezeman-tests-dev-running-first-label ()
  "The service label can be the first label."
  (let* ((freezeman-project-root "/srv/freezeman")
         (freezeman-tests--ps-output "running,com.docker.compose.service=dev,\n")
         (calls (freezeman-tests--with-stubs (freezeman-backend)))
         (processes (freezeman-tests--calls-of 'call-process calls)))
    (should (= (length processes) 1))))

(ert-deftest freezeman-tests-dev-not-running ()
  "If `dev' is stopped or absent, the commands run \"up\" before exec."
  (let ((freezeman-project-root "/srv/freezeman"))
    (dolist (ps-output '(""
                         "exited,com.docker.compose.service=dev,\n"
                         "running,com.docker.compose.service=db,\n"
                         "running,com.docker.compose.service=devx,\n"))
      (let* ((freezeman-tests--ps-output ps-output)
             (calls (freezeman-tests--with-stubs (freezeman-backend)))
             (processes (freezeman-tests--calls-of 'call-process calls))
             (up (nth 1 processes))
             (up-tail (last up 4)))
        (should (equal up-tail '("up" "-d" "--no-recreate" "dev")))
        ;; "up" runs before the server starts.
        (should (eq (caar (last calls)) 'async-shell-command))))))

(ert-deftest freezeman-tests-up-fails ()
  (let ((freezeman-project-root "/srv/freezeman")
        (freezeman-tests--ps-output "")
        (freezeman-tests--up-status 1))
    (should-error (freezeman-tests--with-stubs (freezeman-backend))
                  :type 'user-error))
  (kill-buffer "*freezeman podman-compose up*"))

(ert-deftest freezeman-tests-no-podman-compose ()
  (let ((freezeman-project-root "/srv/freezeman"))
    (cl-letf (((symbol-function 'executable-find) #'ignore))
      (should-error (freezeman--ensure-dev "/srv/freezeman/")
                    :type 'user-error))))

(ert-deftest freezeman-tests-eglot-contact ()
  "The TypeScript server gets tsserver.path, unless the option is nil."
  (let ((freezeman-project-root "/srv/freezeman")
        (freezeman-tsserver-path "/opt/ts/lib"))
    (dolist (case '((python-ts-mode "basedpyright-langserver")
                    (typescript-ts-mode "typescript-language-server"
                                        :initializationOptions
                                        (:tsserver (:path "/opt/ts/lib")))
                    (js-mode "typescript-language-server"
                             :initializationOptions
                             (:tsserver (:path "/opt/ts/lib")))))
      (with-temp-buffer
        (delay-mode-hooks (funcall (car case)))
        (let* ((contact nil)
               (calls (freezeman-tests--with-stubs
                       (setq contact (freezeman-eglot-contact t))))
               (server (nth 1 case))
               (options (nthcdr 2 case)))
          (should (freezeman-tests--calls-of 'call-process calls))
          (should (equal contact
                         (append (list "podman-compose"
                                       "-f" "/srv/freezeman/oci/compose.yaml"
                                       "-f" "/srv/freezeman/oci/compose.dev.yaml"
                                       "exec" "-T" "dev" server "--stdio")
                                 options))))))
    ;; A relative path is relative to the root.
    (with-temp-buffer
      (delay-mode-hooks (js-mode))
      (let* ((freezeman-tsserver-path "frontend/ts")
             (contact nil))
        (freezeman-tests--with-stubs
         (setq contact (freezeman-eglot-contact t)))
        (should (equal (car (last contact))
                       '(:tsserver (:path "/srv/freezeman/frontend/ts"))))))
    (with-temp-buffer
      (delay-mode-hooks (js-mode))
      (let* ((freezeman-tsserver-path nil)
             (contact nil))
        (freezeman-tests--with-stubs
         (setq contact (freezeman-eglot-contact t)))
        (should-not (memq :initializationOptions contact))))))

(ert-deftest freezeman-tests-eglot-setup ()
  (freezeman-tests--with-checkout t
    (let ((freezeman-project-root nil)
          (eglot-server-programs '((foo-mode "foo"))))
      ;; File in the checkout: a local entry for the major mode.
      (with-temp-buffer
        (setq buffer-file-name (file-name-concat root "backend" "a.py"))
        (setq major-mode 'python-ts-mode)
        (freezeman-eglot-setup)
        (should (local-variable-p 'eglot-server-programs))
        (should (equal eglot-server-programs
                       '((python-ts-mode . freezeman-eglot-contact)
                         (foo-mode "foo"))))
        (setq buffer-file-name nil))
      ;; File outside any checkout.
      (with-temp-buffer
        (setq buffer-file-name (file-name-concat temporary-file-directory "a.py"))
        (let ((default-directory temporary-file-directory))
          (cl-letf (((symbol-function 'project-current) #'ignore))
            (freezeman-eglot-setup)))
        (should-not (local-variable-p 'eglot-server-programs))
        (setq buffer-file-name nil)))))

(ert-deftest freezeman-tests-eglot-setup-outside-option-root ()
  "With `freezeman-project-root' set, a file outside it is left alone."
  (let ((freezeman-project-root "/srv/freezeman")
        (eglot-server-programs nil))
    (with-temp-buffer
      (setq buffer-file-name "/home/me/other/a.py")
      (freezeman-eglot-setup)
      (should-not (local-variable-p 'eglot-server-programs))
      (setq buffer-file-name nil))))

;;; freezeman-tests.el ends here
