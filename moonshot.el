;;; moonshot.el --- Run executable file, debug and build commands on project  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Jong-Hyouk Yun

;; Author: Jong-Hyouk Yun <ageldama@gmail.com>
;; URL: https://github.com/ageldama/moonshot
;; Package-Version: 1.0.0
;; Version: 1.0.0
;; Package-Requires: ((cl-lib "0.5") (f "0.18") (s "1.11.0") (projectile "2.0.0") (counsel "0.11.0") (realgud "20190122.43") (seq "2.20") (levenshtein "20090830.1040"))
;; Keywords: convenience, files, processes, tools, unix

;; This file is not part of GNU Emacs
;; See `LICENSE'

;;; Commentary:

;; << Project Build Directory >>
;;
;; 1) File local variable `moonshot:project-build-dir', and if it is:
;;   1) a string starts with `/', use it,
;;   2) a string and does not starts with `/',
;;      - Append it to project root directory or the directory of current buffer.
;;      - If the directory of current buffer is not available, it's nil.
;;   3) a list, returns the value of `eval'-ed on it.
;;   4) Implemented in `moonshot:project-build-dir-by-value' function.
;;
;; 2) Ask to Projectile
;;
;; 3) Just the directory of current buffer

;; << Launch Executable >>
;;
;;  `moonshot:run-executable'
;;
;;  1) Will search executable files under `moonshot:project-build-dir'.
;;  2) Suggest similar executable files first from buffer filename.

;; << Launch Debugger with Executable >>
;;
;;  `moonshot:run-debugger'
;;
;;  - Use same way with `moonshot:run-executable' to choose an executable to debug.
;;  - Supported debuggers are listed at `moonshot:debuggers'.

;; << Run Shell Command in Compilation-Mode >>
;;
;;  `moonshot:run-runner'
;;
;;  - Global shell command presets are `moonshot:runners-preset'.
;;  - Per project commands can be added to `moonshot:runners', by specifying variable in `.dir-locals.el' etc.
;;
;;   <<< Command String Expansion >>>
;;    - Following format specifiers are will expanded in command string:
;;      %a  absolute pathname            ( /usr/local/bin/netscape.bin )
;;      %f  file name without directory  ( netscape.bin )
;;      %n  file name without extension  ( netscape )
;;      %e  extension of file name       ( bin )
;;      %d  directory                    ( /usr/local/bin/ )
;;      %p  project root directory       ( /home/who/blah/ ), using Projectile
;;      %b  project build directory      ( /home/who/blah/build/ ), using `moonshot:project-build-dir'"


;;; Code:
(eval-when-compile
  (require 'cl))

(require 'cl-lib)
(require 'f)
(require 's)
(require 'ivy)
(require 'realgud)
(require 'seq)
(require 'levenshtein)
(require 'projectile)

;;; --- Variables

(defcustom moonshot:project-build-dir nil
  "Project build directory.
Can be a string or a form."
  :group 'moonshot
  :type 'sexp)

(defcustom moonshot:debuggers
  '(
    ;; `COMMAND' . `DEBUGGER-FN'
    ("gdb #realgud" . realgud:gdb)
    ("gdb #gud" . gud-gdb)
    ("lldb #realgud" . realgud:lldb)
    ("python -mpdb #realgud" . realgud:pdb)
    ("perldb #realgud" . realgud:perldb)
    ("pydb #realgud" . realgud:pydb)
    ("gub #realgud" . realgud:gub)
    ("jdb #realgud" . realgud:jdb)
    ("bashdb #realgud" . realgud:bashdb)
    ("remake #realgud" . realgud:remake)
    ("zshdb #realgud" . realgud:zshdb)
    ("kshdb #realgud" . realgud:kshdb)
    ("dgawk #realgud" . realgud:dgawk)
    )
  "Supported debuggers."
  :group 'moonshot
  :type '(alist :key string :value function))

(defcustom moonshot:runners-preset
  '("cmake -S\"%p\" -B\"%b\" -GNinja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=on"
    "cd \"%b\"; ninja"
    )
  "Available shell command presets."
  :group 'moonshot
  :type '(list string))

(defcustom moonshot:runners
  nil
  "Shell commands for file variables / `.dir-locals.el'."
  :group 'moonshot
  :type '(list string))
  



;;; --- Project Build Directory
(defun moonshot:project-build-dir-by-value (val)
  "Composes the build directory path string by `VAL'."
  (let ((path (cl-typecase val
                (string (if (s-starts-with? "/" val)
                            val ; absolute-path
                          ;; relative-path
                          (when-let ((it buffer-file-name))
                            (s-concat (or (projectile-project-root)
                                          (file-name-directory it)) val))))
                (t (eval val)))))
    (when-let ((it path))
      (unless (f-exists? it)
        (error "Invalid path or File/directory not found: %s" it)))
    (expand-file-name path)))

(defun moonshot:project-build-dir ()
  "Find the build directory by one of following methods sequentially:

1) File local variable `moonshot:project-build-dir', and if it is:
  1) a string starts with `/', use it,
  2) a string and does not starts with `/',
     - Append it to project root directory or the directory of current buffer.
     - If the directory of current buffer is not available, it's nil.
  3) a list, returns the value of `eval'-ed on it.
  4) Implemented in `moonshot:project-build-dir-by-value' function.

2) Ask to Projectile

3) Just the directory of current buffer

Thus, can be evaluated as nil on some special buffers.
For example, `*scratch*'-buffer"
  (or (when-let ((it moonshot:project-build-dir)) ; file local variable
        (moonshot:project-build-dir-by-value it))
      (projectile-project-root)
      (when-let ((it buffer-file-name))
        (file-name-directory it))))


;;; --- Run/Debug
(defun moonshot:list-executable-files (dir)
  "Find every executable files under `DIR'.
Evaluates as nil when `DIR' is nil."
  (if dir
      (seq-filter 'file-executable-p
                  (directory-files-recursively
                   dir ".*"))
    ;; `dir'=nil => empty
    nil))

(defun* moonshot:file-list->distance-alist (fn file-names &key dist-fun)
  "Calculate string difference distances from `FN' of given `FILE-NAMES'using `DIST-FUN'.
Evaluates as nil when `FILE-NAMES' is nil."
  (block file-list->dist-alist
    (unless file-names
      (return-from file-list->dist-alist nil))
    (let ((fn* (f-filename fn)))
      (mapcar (lambda (i)
                (cons (funcall (or dist-fun #'levenshtein-distance)
                               fn* (f-filename i))
                      i))
              file-names))))

(defun moonshot:list-executable-files-and-sort-by (dir file-name)
  "Find every executable file names under `DIR'.
The list is sorted by `file-list->distance-alist' with `FILE-NAME'."
  (mapcar #'cdr
          (sort
           (moonshot:file-list->distance-alist
            file-name
            (moonshot:list-executable-files dir))
           (lambda (x y) (< (car x) (car y))))))
;; Try: (list-executable-files-and-sort-by "/bin" "sh")

;;;###autoload
(defun moonshot:run-command-with (cmd mkcmd-fun run-fun)
  "Read and Run with `RUN-FUN' and pass `CMD' filtered by `MKCMD-FUN' as parameter."
  (interactive)
  (let* ((cmd*
          (read-from-minibuffer "Cmd: " (funcall mkcmd-fun cmd))))
    (funcall run-fun cmd*)))


;;;###autoload
(defun moonshot:run-executable ()
  "Select an executable file in `moonshot:project-build-dir', similar to buffer filename."
  (interactive)
  (let ((fn (buffer-file-name)))
    (ivy-read "Select an executable to run: "
              (moonshot:list-executable-files-and-sort-by (moonshot:project-build-dir) fn)
              :action (lambda (cmd)
                        (moonshot:run-command-with cmd
                                                   (lambda (cmd)
                                                     (format "cd '%s'; '%s'" (f-dirname cmd) cmd))
                                                   #'compile)))))




 
(defun moonshot:alist-keys (l)
  "CARs of an Alist `L'."
  (cl-loop for i in l collect (car i)))

(defvar moonshot:run-debugger/history nil)


(defun moonshot:%remove-sharp-comment (s)
  "Remove shell comment section from string `S'."
  (s-trim (replace-regexp-in-string  "\\#.*$" "" s)))


;;;###autoload
(defun moonshot:run-debugger (debugger)
  "Launch debugger, one of `moonshot:debuggers', with an executable selection.

`DEBUGGER' is member of `moonshot:debuggers'"
  (interactive (list (ivy-read "Select debugger: "
                               (moonshot:alist-keys moonshot:debuggers)
                               :require-match t
                               :history 'moonshot:run-debugger/history)))
  (let ((fn (buffer-file-name))
        (debugger-func (cdr (assoc debugger moonshot:debuggers))))
    (ivy-read "Select an executable to debug: "
              (moonshot:list-executable-files-and-sort-by (moonshot:project-build-dir) fn)
              :action (lambda (cmd)
                        (moonshot:run-command-with cmd
                                                   (lambda (cmd)
                                                     (format "\"%s\" \"%s\""
                                                             (moonshot:%remove-sharp-comment debugger) cmd))
                                                   debugger-func)))))


;;; Runner
(defun moonshot:all-runners ()
  "Collect available runners."
  (append moonshot:runners moonshot:runners-preset))

(defun moonshot:expand-path-vars (path-str)
  "Expand `PATH-STR' with following format specifiers.

%a  absolute pathname            ( /usr/local/bin/netscape.bin )
%f  file name without directory  ( netscape.bin )
%n  file name without extension  ( netscape )
%e  extension of file name       ( bin )
%d  directory                    ( /usr/local/bin/ )
%p  project root directory       ( /home/who/blah/ ), using Projectile
%b  project build directory      ( /home/who/blah/build/ ), using `moonshot:project-build-dir'"
  (let* ((s path-str)
         (abs-path (or (buffer-file-name) ""))
         (file-name "")
         (file-name-without-ext "")
         (file-ext "")
         (dir "")
         (project-root-dir (projectile-project-root))
         (project-build-dir (moonshot:project-build-dir)))
    ;; fill in
    (unless (s-blank-str? abs-path)
      (setq file-name (or (f-filename abs-path) "")
            file-ext (or (f-ext file-name) "")
            dir (or (f-dirname abs-path) ""))
      (unless (and (s-blank-str? dir)
                   (s-suffix? "/" dir))
        (setq dir (s-concat dir "/")))
      (setq file-name-without-ext (s-chop-suffix (if (not (s-blank-str? file-ext))
                                                     (s-concat "." file-ext)
                                                   file-ext)
                                                 file-name)))
    ;; pattern -> replacement
    (dolist (pattern->replacement `(("%a" . ,abs-path)
                                    ("%f" . ,file-name)
                                    ("%n" . ,file-name-without-ext)
                                    ("%e" . ,file-ext)
                                    ("%d" . ,dir)
                                    ("%p" . ,project-root-dir)
                                    ("%b" . ,project-build-dir)))
      (let ((case-fold-search nil)
            (pattern (car pattern->replacement))
            (replacement (cdr pattern->replacement)))
        (setq s (replace-regexp-in-string pattern replacement s t t))))
    ;;
    s))


(defvar moonshot:run-runner/history nil)

;;;###autoload
(defun moonshot:run-runner ()
  "Run runner."
  (interactive)
  (ivy-read "Command: "
            (moonshot:all-runners)
            :action (lambda (cmd)
                      (moonshot:run-command-with cmd
                                                 #'moonshot:expand-path-vars
                                                 #'compile))
            :history 'moonshot:run-runner/history))



(provide 'moonshot)
;;; moonshot.el ends here
