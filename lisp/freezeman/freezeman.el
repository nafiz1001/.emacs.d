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
;; The backend commands expect a virtual environment in backend/.venv.
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

;; lisp/freezeman/freezeman-tests.el tests these commands.

;;;###autoload
(defun freezeman-backend ()
  "Run the Django development server of the Freezeman backend.
Kill the buffer of an earlier server first."
  (interactive)
  (let* ((root (freezeman--root))
         (backend-dir (file-name-concat root "backend"))
         (command (concat "cd " backend-dir
                          " && . ./.venv/bin/activate"
                          " && python manage.py runserver"))
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
         (frontend-dir (file-name-concat root "frontend"))
         (command (concat "cd " frontend-dir " && npm start"))
         (buffer (get-buffer "npm start")))
    (when buffer
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (async-shell-command command "npm start")))

;;;###autoload
(defun freezeman-shell ()
  "Run the Django shell of the Freezeman backend in an inferior Python."
  (interactive)
  (let* ((root (freezeman--root))
         (default-directory root)
         (python (file-name-concat root "backend" ".venv" "bin" "python"))
         (manage (file-name-concat "backend" "manage.py"))
         (command (concat python " " manage " shell")))
    (run-python command nil t)))

(provide 'freezeman)

;;; freezeman.el ends here
