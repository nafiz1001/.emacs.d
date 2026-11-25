;;; freezeman-tests.el --- Tests for freezeman.el -*- lexical-binding: t -*-

;;; Commentary:

;; Run from the root of the Emacs configuration:
;;
;;   emacs -Q --batch -L lisp/freezeman -l lisp/freezeman/freezeman-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'freezeman)

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
  "Run BODY with `async-shell-command' and `run-python' stubbed.
Bind `calls' to the list of (FUNCTION . ARGS) in call order, with
`default-directory' at the call as the last element of ARGS."
  `(let ((calls nil))
     (cl-letf (((symbol-function 'async-shell-command)
                (lambda (&rest args)
                  (push (append (list 'async-shell-command) args
                                (list default-directory))
                        calls)))
               ((symbol-function 'run-python)
                (lambda (&rest args)
                  (push (append (list 'run-python) args
                                (list default-directory))
                        calls))))
       ,@body
       (setq calls (nreverse calls)))
     calls))

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
      (should-error (freezeman--root) :type 'user-error))))

(ert-deftest freezeman-tests-option-wins ()
  (freezeman-tests--with-checkout t
    (let ((freezeman-project-root "/srv/freezeman"))
      (should (equal (freezeman--root) "/srv/freezeman/")))))

(ert-deftest freezeman-tests-backend-command ()
  (let* ((system-type 'gnu/linux)
         (freezeman-project-root "/srv/freezeman")
         (calls (freezeman-tests--with-stubs (freezeman-backend))))
    (should (equal (car calls)
                   '(async-shell-command
                     "/srv/freezeman/backend/.venv/bin/python manage.py runserver"
                     "python manage.py runserver"
                     "/srv/freezeman/backend/")))))

(ert-deftest freezeman-tests-backend-command-windows ()
  "On native Windows, the backend runs Scripts\\python.exe of the venv.
The space in the root tests the quoting for cmd.exe."
  (cl-letf (((symbol-function 'w32-shell-dos-semantics) (lambda () t)))
    (let* ((system-type 'windows-nt)
           (freezeman-project-root "/srv/free man")
           (calls (freezeman-tests--with-stubs (freezeman-backend))))
      (should (equal (car calls)
                     '(async-shell-command
                       "\"/srv/free man/backend/.venv/Scripts/python.exe\" manage.py runserver"
                       "python manage.py runserver"
                       "/srv/free man/backend/"))))))

(ert-deftest freezeman-tests-frontend-command ()
  (let* ((freezeman-project-root "/srv/freezeman")
         (calls (freezeman-tests--with-stubs (freezeman-frontend))))
    (should (equal (car calls)
                   '(async-shell-command
                     "npm start"
                     "npm start"
                     "/srv/freezeman/frontend/")))))

(ert-deftest freezeman-tests-kill-old-buffer ()
  (let ((freezeman-project-root "/srv/freezeman"))
    (dolist (case '((freezeman-backend . "python manage.py runserver")
                    (freezeman-frontend . "npm start")))
      (let ((old (get-buffer-create (cdr case))))
        (freezeman-tests--with-stubs (funcall (car case)))
        (should-not (buffer-live-p old)))
      ;; No old buffer: the command must not signal an error.
      (freezeman-tests--with-stubs (funcall (car case))))))

(ert-deftest freezeman-tests-shell-command ()
  (let* ((system-type 'gnu/linux)
         (freezeman-project-root "/srv/freezeman")
         (calls (freezeman-tests--with-stubs (freezeman-shell))))
    (should (equal (car calls)
                   '(run-python
                     "/srv/freezeman/backend/.venv/bin/python backend/manage.py shell"
                     nil t "/srv/freezeman/")))))

(ert-deftest freezeman-tests-shell-command-windows ()
  "On native Windows, the shell runs Scripts\\python.exe of the venv."
  (cl-letf (((symbol-function 'w32-shell-dos-semantics) (lambda () t)))
    (let* ((system-type 'windows-nt)
           (freezeman-project-root "/srv/free man")
           (calls (freezeman-tests--with-stubs (freezeman-shell))))
      (should (equal (car calls)
                     '(run-python
                       "\"/srv/free man/backend/.venv/Scripts/python.exe\" backend/manage.py shell"
                       nil t "/srv/free man/"))))))

;;; freezeman-tests.el ends here
