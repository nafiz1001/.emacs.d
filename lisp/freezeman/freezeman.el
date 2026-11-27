;;; freezeman.el --- Commands for Freezeman development -*- lexical-binding: t -*-

;; Author: Nafiz Islam
;;         Claude Opus 5.5 <noreply@anthropic.com>

;;; Commentary:

;; Commands to run the parts of a Freezeman checkout
;; (https://github.com/c3g/freezeman) from Emacs:
;;
;; - `freezeman-backend': Django development server
;;   ("python manage.py runserver").
;; - `freezeman-frontend': "npm start" in the frontend.
;; - `freezeman-shell': Django shell in an inferior Python buffer.
;; - `freezeman-eglot-setup': run the language servers through Eglot.
;;
;; All of them run in the `dev' container of oci/compose.dev.yaml
;; (see oci/DEVELOPMENT.md of the checkout).  They use
;; "podman-compose exec" with oci/compose.yaml, oci/compose.dev.yaml
;; and, if it exists, oci/compose.local.yaml.  If `dev' does not run,
;; they start it first with "podman-compose up -d --no-recreate dev".
;; If it runs, they reuse it.
;;
;; Eglot sends host file names to the servers.  So the container must
;; mount the checkout at its host path: set CONTAINER_FMS_ROOT in
;; oci/.env to the absolute path of the checkout, then rebuild the
;; images.  The servers (basedpyright-langserver and
;; typescript-language-server) must be on the PATH of `dev', for
;; example through oci/compose.local.yaml.
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
;;     :commands (freezeman-backend freezeman-frontend freezeman-shell
;;                freezeman-eglot-setup)
;;     :hook ((python-base-mode js-base-mode typescript-ts-base-mode)
;;            . freezeman-eglot-setup)
;;     :custom
;;     (freezeman-project-root "~/projects/freezeman/"))

;;; Code:

(require 'project)
(require 'subr-x)

(defvar eglot-server-programs)
(defvar python-shell-prompt-detect-enabled)

(defgroup freezeman nil
  "Commands for Freezeman development."
  :group 'tools)

(defcustom freezeman-project-root nil
  "Root directory of the Freezeman checkout.
If nil, the commands use the root of the current project if it is a
Freezeman checkout."
  :type '(choice (const :tag "Current project" nil) directory))

;; typescript-language-server looks for TypeScript only in
;; node_modules of the project root, but node_modules is in frontend/.
;; The default uses the TypeScript of the frontend, the version that
;; builds it.  TypeScript 7 has no tsserver.
(defcustom freezeman-tsserver-path "frontend/node_modules/typescript/lib"
  "Directory of tsserver in the `dev' container, or nil.
Eglot passes it to typescript-language-server as tsserver.path.  A
relative name is relative to the root of the Freezeman checkout,
which has the same path in the container."
  :type '(choice (const :tag "Server default" nil) string))

;; python.el sends code through temporary files, which the container
;; must see.  logs/ is git-ignored.
(defcustom freezeman-shell-temp-directory "logs/emacs-tmp/"
  "Directory of the temporary files of `freezeman-shell'.
It must be in the checkout, which has the same path in the
container.  A relative name is relative to the root of the Freezeman
checkout."
  :type 'directory)

(defun freezeman--find-root ()
  "Return the root directory of the Freezeman checkout, or nil.
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
      (when (and settings
                 (file-readable-p settings)
                 (with-temp-buffer
                   (insert-file-contents settings)
                   (search-forward "fms_core.apps.FmsCoreConfig" nil t)))
        (let ((expanded (expand-file-name root)))
          (file-name-as-directory expanded))))))

(defun freezeman--root ()
  "Return the root directory of the Freezeman checkout.
Signal a `user-error' if there is none."
  (or (freezeman--find-root)
      (user-error "Not in a Freezeman project.  Set `freezeman-project-root'")))

(defun freezeman--compose-args (root)
  "Return the \"-f FILE\" arguments of podman-compose for ROOT.
The files are oci/compose.yaml, oci/compose.dev.yaml and, if it
exists, oci/compose.local.yaml."
  (let* ((oci (file-name-concat root "oci"))
         (base (list (file-name-concat oci "compose.yaml")
                     (file-name-concat oci "compose.dev.yaml")))
         (local (file-name-concat oci "compose.local.yaml"))
         (files (if (file-exists-p local)
                    (append base (list local))
                  base)))
    (mapcan (lambda (file) (list "-f" file)) files)))

;; lisp/freezeman/freezeman-tests.el tests this function.
(defun freezeman--ensure-dev (root)
  "Start the `dev' container of ROOT if it does not run."
  (unless (executable-find "podman-compose")
    (user-error "podman-compose is not installed"))
  (let* ((compose-args (freezeman--compose-args root))
         ;; podman-compose ps has no --filter option.  It lists the
         ;; containers of the project, also the stopped ones.  The
         ;; labels are one comma-separated string.
         (ps-args (append compose-args
                          (list "ps" "--format" "{{.State}},{{.Labels}},")))
         (ps-output (with-temp-buffer
                      (apply #'call-process "podman-compose" nil '(t nil) nil
                             ps-args)
                      (buffer-string))))
    (unless (string-match-p
             "^running,\\(?:.*,\\)?com\\.docker\\.compose\\.service=dev,"
             ps-output)
      (message "Starting the Freezeman dev container...")
      (let* ((buffer (get-buffer-create "*freezeman podman-compose up*"))
             ;; Without --no-recreate, up recreates the containers whose
             ;; configuration changed.
             (up-args (append compose-args
                              (list "up" "-d" "--no-recreate" "dev")))
             (status (with-current-buffer buffer
                       (erase-buffer)
                       (apply #'call-process "podman-compose" nil t nil
                              up-args))))
        (unless (eql status 0)
          (display-buffer buffer)
          (user-error "podman-compose up failed.  See the buffer %s"
                      (buffer-name buffer)))))))

(defun freezeman--dev-words (root &rest args)
  "Return the words of a command that runs ARGS in `dev' of ROOT."
  (let ((compose-args (freezeman--compose-args root)))
    (append (list "podman-compose") compose-args (list "exec" "dev") args)))

(defun freezeman--dev-shell-command (root &rest args)
  "Return a shell command that runs ARGS in the `dev' container of ROOT."
  (let* ((words (apply #'freezeman--dev-words root args))
         (quoted (mapcar #'shell-quote-argument words)))
    (string-join quoted " ")))

(defun freezeman--kill-buffer (name)
  "Kill the buffer NAME if it exists, with no question."
  (let ((buffer (get-buffer name)))
    (when buffer
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

;; lisp/freezeman/freezeman-tests.el tests these commands with stubs.
;; lisp/freezeman/freezeman-live-tests.el tests them, and the Eglot
;; functions below, in a real `dev' container.
;;
;; The commands quote each word with `shell-quote-argument', because
;; `async-shell-command' uses cmd.exe on native Windows.  The servers
;; listen on 0.0.0.0, because the published ports connect to the
;; container IP.  The working directory in `dev' is the root of the
;; checkout.

;;;###autoload
(defun freezeman-backend ()
  "Run the Django development server of the Freezeman backend.
Kill the buffer of an earlier server first."
  (interactive)
  (let* ((root (freezeman--root))
         (default-directory root)
         (command (freezeman--dev-shell-command
                   root "python" "backend/manage.py" "runserver" "0.0.0.0:8000")))
    (freezeman--ensure-dev root)
    (freezeman--kill-buffer "python manage.py runserver")
    (async-shell-command command "python manage.py runserver")))

;;;###autoload
(defun freezeman-frontend ()
  "Run \"npm start\" in the Freezeman frontend.
Kill the buffer of an earlier run first."
  (interactive)
  (let* ((root (freezeman--root))
         (default-directory root)
         (command (freezeman--dev-shell-command
                   root "npm" "--prefix" "frontend" "start" "--" "--host" "0.0.0.0")))
    (freezeman--ensure-dev root)
    (freezeman--kill-buffer "npm start")
    (async-shell-command command "npm start")))

;;;###autoload
(defun freezeman-shell ()
  "Run the Django shell of the Freezeman backend in an inferior Python.
To restart the shell, kill its buffer and run this command again.
`python-shell-restart' loses the container settings."
  (interactive)
  (let* ((root (freezeman--root))
         (default-directory root)
         ;; exec gives the container a TTY with echo on, so python.el
         ;; would read its own input back as output.
         (words (freezeman--dev-words
                 root "sh" "-c"
                 "stty -echo && exec python backend/manage.py shell"))
         ;; run-python splits its command with `split-string-and-unquote',
         ;; not with a shell, so quote with its inverse.
         (command (combine-and-quote-strings words))
         (temp-directory (expand-file-name freezeman-shell-temp-directory
                                           root)))
    (freezeman--ensure-dev root)
    (make-directory temp-directory t)
    (let* ((process
            ;; Prompt detection runs the first word of the command,
            ;; podman-compose, as a Python interpreter.  It fails and
            ;; warns.  The default prompt regexps match the Django shell.
            (let ((python-shell-prompt-detect-enabled nil))
              (run-python command nil t)))
           (buffer (process-buffer process)))
      ;; python.el makes its temporary files in the process buffer.
      ;; The process output, and so the setup code that python.el sends
      ;; at the first prompt, is read only after this command returns.
      (with-current-buffer buffer
        (setq-local temporary-file-directory
                    (file-name-as-directory temp-directory))))))

;; lisp/freezeman/freezeman-tests.el tests the Eglot functions.
;;;###autoload
(defun freezeman-eglot-contact (&optional _interactive)
  "Return the Eglot contact of the language server in `dev'.
Start `dev' first if it does not run."
  (let* ((root (freezeman--root))
         (python (derived-mode-p 'python-base-mode))
         (server (if python
                     "basedpyright-langserver"
                   "typescript-language-server"))
         (compose-args (freezeman--compose-args root))
         ;; -T: no pseudo-TTY.  A TTY would change the bytes of the LSP
         ;; stream.
         (command (append (list "podman-compose") compose-args
                          (list "exec" "-T" "dev" server "--stdio"))))
    (freezeman--ensure-dev root)
    (if (or python (not freezeman-tsserver-path))
        command
      (let* ((path (expand-file-name freezeman-tsserver-path root))
             (options (list :tsserver (list :path path))))
        (append command (list :initializationOptions options))))))

;;;###autoload
(defun freezeman-eglot-setup ()
  "Make Eglot run the language server of this buffer in `dev'.
Do nothing if the file of the buffer is not in a Freezeman checkout.
Put this function on the hooks of the Python and JavaScript/TypeScript
modes."
  (let ((root (freezeman--find-root)))
    (when (and root
               buffer-file-name
               (file-in-directory-p buffer-file-name root))
      ;; A buffer-local value, so other projects keep the global
      ;; servers.  Eglot reads the variable in the buffer.
      (let ((entry (cons major-mode #'freezeman-eglot-contact))
            (global (bound-and-true-p eglot-server-programs)))
        (setq-local eglot-server-programs (cons entry global))))))

(provide 'freezeman)

;;; freezeman.el ends here
