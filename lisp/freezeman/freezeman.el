;;; freezeman.el --- Commands for Freezeman development -*- lexical-binding: t -*-

;; Author: Nafiz Islam
;;         Claude Opus 5.5 <noreply@anthropic.com>

;;; Commentary:

;; Commands to run the parts of a Freezeman checkout
;; (https://github.com/c3g/freezeman) from Emacs:
;;
;; - `freezeman-backend': Django development server in
;;   "python manage.py runserver".
;; - `freezeman-frontend': "npm start" in the frontend.
;; - `freezeman-shell': Django shell in an inferior Python buffer.
;;
;; The backend commands use the virtual environment in
;; `freezeman-venv-directory' (default backend/env).
;; The commands work on Linux, macOS and native Windows.
;;
;; The commands find the checkout as follows:
;; 1. If `freezeman-project-root' is set, they use it.
;; 2. Else, they use the root of the current project, if it is a
;;    Freezeman checkout.
;;
;; Example with use-package:
;;
;;   (use-package freezeman
;;     :load-path "path/to/freezeman"
;;     :commands (freezeman-backend freezeman-frontend freezeman-shell)
;;     :custom
;;     (freezeman-project-root "~/projects/freezeman/"))

;;; Code:

(require 'project)

(defgroup freezeman nil
  "Commands for Freezeman development."
  :group 'tools)

(defcustom freezeman-project-root nil
  "Root directory of the Freezeman checkout.
If nil, the commands use the root of the current project if it is a
Freezeman checkout."
  :type '(choice (const :tag "Current project" nil) directory))

;; lisp/freezeman/freezeman-tests.el tests this option.
(defcustom freezeman-venv-directory "backend/env"
  "Directory of the virtual environment of the backend.
A relative name is relative to the root of the Freezeman checkout.
An absolute name is used as it is."
  :type 'directory)

(defun freezeman--root ()
  "Return the root directory of the Freezeman checkout.
Use `freezeman-project-root' if it is set.  Else use the root of the
current project if it is a Freezeman checkout."
  (if freezeman-project-root
      (let ((root (expand-file-name freezeman-project-root)))
        (file-name-as-directory root))
    (let* ((project (project-current))
           (root (and project (project-root project)))
           (settings (and root
                          (file-name-concat root "backend" "fms" "settings.py"))))
      ;; Freezeman lists this app in INSTALLED_APPS.  Forks and
      ;; worktrees have other remote URLs and directory names, so
      ;; neither identifies a checkout.
      (if (and settings
               (file-readable-p settings)
               (with-temp-buffer
                 (insert-file-contents settings)
                 (search-forward "fms_core.apps.FmsCoreConfig" nil t)))
          (let ((expanded (expand-file-name root)))
            (file-name-as-directory expanded))
        (user-error "Not in a Freezeman project.  Set `freezeman-project-root'")))))

(defun freezeman--venv-python (root)
  "Return the Python of the virtual environment of ROOT.
`freezeman-venv-directory' gives the venv.  A venv on native Windows
has Scripts\\python.exe.  Elsewhere it has bin/python."
  (let ((venv (expand-file-name freezeman-venv-directory root)))
    (if (eq system-type 'windows-nt)
        (file-name-concat venv "Scripts" "python.exe")
      (file-name-concat venv "bin" "python"))))

;; lisp/freezeman/freezeman-tests.el tests these commands.
;;
;; The commands set `default-directory' instead of a "cd DIR &&" prefix,
;; and run the Python of the venv instead of sourcing its activate
;; script.  The activate script needs a POSIX shell, and
;; `async-shell-command' uses cmd.exe on native Windows.

;;;###autoload
(defun freezeman-backend ()
  "Run the Django development server of the Freezeman backend.
Kill the buffer of an earlier server first."
  (interactive)
  (let* ((root (freezeman--root))
         (default-directory (file-name-concat root "backend/"))
         (python (freezeman--venv-python root))
         (quoted-python (shell-quote-argument python))
         (command (concat quoted-python " manage.py runserver"))
         (buffer (get-buffer "python manage.py runserver")))
    (when buffer
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (async-shell-command command "python manage.py runserver")))

;;;###autoload
(defun freezeman-frontend ()
  "Run \"npm start\" in the Freezeman frontend.
Kill the buffer of an earlier run first."
  (interactive)
  (let* ((root (freezeman--root))
         (default-directory (file-name-concat root "frontend/"))
         (buffer (get-buffer "npm start")))
    (when buffer
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (async-shell-command "npm start" "npm start")))

;;;###autoload
(defun freezeman-shell ()
  "Run the Django shell of the Freezeman backend in an inferior Python."
  (interactive)
  (let* ((root (freezeman--root))
         (default-directory root)
         (python (freezeman--venv-python root))
         (quoted-python (shell-quote-argument python))
         (manage (file-name-concat "backend" "manage.py"))
         (command (concat quoted-python " " manage " shell")))
    (run-python command nil t)))

(provide 'freezeman)

;;; freezeman.el ends here
