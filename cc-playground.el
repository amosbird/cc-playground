;;; cc-playground.el --- Local C/C++ playground for short snippets.

;; Copyright (C) 2017-2018 Amos Bird
;;   ___                       ______ _         _
;;  / _ \                      | ___ (_)       | |
;; / /_\ \_ __ ___   ___  ___  | |_/ /_ _ __ __| |
;; |  _  | '_ ` _ \ / _ \/ __| | ___ \ | '__/ _` |
;; | | | | | | | | | (_) \__ \ | |_/ / | | | (_| |
;; \_| |_/_| |_| |_|\___/|___/ \____/|_|_|  \__,_|

;; Author: Amos Bird <amosbird@gmail.com>
;; URL: https://github.com/amosbird/cc-playground
;; Keywords: tools, c/c++
;; Version: 1.1
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Local playground for C/C++ programs.
;; `M-x cc-playground` and type you code then make&run it with `C-Return`.

;;

;;; Code:

(require 'compile)
(require 'time-stamp)

(defgroup cc-playground nil
  "Options specific to C++ Playground."
  :group 'c)

(defcustom cc-playground-ask-file-name nil
  "Non-nil means we ask for a name for the snippet.

By default it will be created as snippet.cpp"
  :type 'boolean
  :group 'cc-playground)

(defcustom cc-playground-confirm-deletion t
  "Non-nil prompts confirmation on the snippet deletion with `cc-playground-rm'.

By default confirmation required."
  :type 'boolean
  :group 'cc-playground)

(defcustom cc-playground-basedir "~/cc/playground"
  "Base directory for playground snippets."
  :type 'file
  :group 'cc-playground)

(defcustom cc-template "
#include <iostream>

using namespace std;

int mymain(int argc, char *argv[]) {
    return 0;
}"
  "Default template for playground."
  :type 'string
  :group 'cc-playground)

(defcustom cc-playground-hook nil
  "Hook when entering playground."
  :type 'hook
  :group 'cc-playground)

(defcustom cc-playground-rm-hook nil
  "Hook when leaving playground."
  :type 'hook
  :group 'cc-playground)

(defvar cc-debug-command "CXX=\"%s\" CXXFLAGS=\"%s\" LDFLAGS=\"%s\" make deb && (tmux switch-client -t amos; tmux run -t amos \"fish -c 'tmuxgdb -ex=start ./deb'\")")
(defvar cc-debug-test-command "CXX=\"%s\" CXXFLAGS=\"%s\" LDFLAGS=\"%s\" make deb_test && (tmux switch-client -t amos; tmux run -t amos \"fish -c 'tmuxgdb -ex=start ./deb_test'\")")
(defvar cc-release-command "CXX=\"%s\" CXXFLAGS=\"%s\" LDFLAGS=\"%s\" make rel && printf \"\\n--------------------\\n\\n\" && ./rel")
(defvar cc-release-test-command "CXX=\"%s\" CXXFLAGS=\"%s\" LDFLAGS=\"%s\" make rel_test && printf \"\\n--------------------\\n\\n\" && ./rel_test")
(defvar cc-bench-command "CXX=\"%s\" CXXFLAGS=\"%s\" LDFLAGS=\"%s\" make bench && printf \"\\n--------------------\\n\\n\" && ./bench  --benchmark_color=false")
;; (defvar cc-release-command "( [ './rel' -nt *.cpp ] || %s -std=c++17 *.cpp -o rel %s) && ./rel")

(defun cc-switch-between-src-and-test ()
  "Switch between src and test file."
  (interactive)
  (let ((name (file-name-base (buffer-file-name))))
    (if (string= name "snippet")
        (find-file (concat default-directory "test.cpp"))
      (if (string= name "test")
          (find-file (concat default-directory "snippet.cpp"))))))

(define-minor-mode cc-playground-mode
  "A place for playing with c++ code."
  :init-value nil
  :lighter "Play(C/C++)"
  :keymap '(([C-return] . cc-playground-exec)
            ([M-return] . cc-playground-exec-test)
            ([?\M-\r]   . cc-playground-exec-test)
            ([S-return] . cc-playground-rm)))

(defun cc-playground-snippet-file-name(&optional snippet-name)
  "Get snippet file name from SNIPPET-NAME. Generate a random one if nil."
  (let ((file-name (cond (snippet-name)
                         (cc-playground-ask-file-name
                          (read-string "C++ Playground filename: "))
                         ("snippet"))))
    (concat (cc-playground-snippet-unique-dir file-name) "/" file-name ".cpp")))

(defvar cc-flags)
(defvar cc-links)
(defvar cc-exec)

(defun cc-playground-run (comm)
  "COMM."
  (if (cc-playground-inside)
      (progn
        (save-buffer t)
        (make-local-variable 'compile-command)
        (let ((include-dirs (getenv "cquery_include_dirs"))
              (cc-flags cc-flags))
          (dolist (s (split-string include-dirs))
            (setq cc-flags (concat cc-flags " -I" s " ")))
          (pcase comm
            ('exec
             (compile (format cc-release-command cc-exec cc-flags cc-links)))
            ('debug
             (compile (format cc-debug-command cc-exec cc-flags cc-links)))
            ('test
             (compile (format cc-release-test-command cc-exec cc-flags cc-links)))
            ('debug-test
             (compile (format cc-debug-test-command cc-exec cc-flags cc-links)))
            ('bench
             (compile (format cc-bench-command cc-exec cc-flags cc-links))))))))

(defun cc-playground-exec ()
  "Save the buffer then run clang compiler for executing the code."
  (interactive)
  (cc-playground-run 'exec))

(defun cc-playground-debug ()
  "Save the buffer then run tmuxgdb for debugging the code."
  (interactive)
  (cc-playground-run 'debug))

(defun cc-playground-exec-test ()
  "Save the buffer then run clang compiler for executing the test."
  (interactive)
  (cc-playground-run 'test))

(defun cc-playground-debug-test ()
  "Save the buffer then run tmuxgdb for debugging the test."
  (interactive)
  (cc-playground-run 'debug-test))

(defun cc-playground-bench ()
  "Save the buffer then run clang compiler for executing the test."
  (interactive)
  (cc-playground-run 'bench))

(defun cc-playground-add-or-modify-tag (name)
  "Adding or modifying existing tag of a snippet using NAME."
  (interactive "MTag Name: ")
  (if (cc-playground-inside)
      (let* ((oname (string-trim-right (shell-command-to-string (concat "basename " default-directory))))
             (nn (concat default-directory "../"))
             (l (split-string oname ":")))
        (fundamental-mode) ;; weird bug when renaming directory
        (if (= (length l) 1)
            (dired-rename-file default-directory (concat nn name ":" oname) nil)
          (dired-rename-file default-directory (concat nn name ":" (cadr l)) nil)))))

;;;###autoload
(defun cc-playground-find-snippet ()
  "List all snippets using `ivy-read'."
  (interactive)
  (ivy-read "Browse cc snippet: "
            (mapcar (lambda (a) (cons (file-name-nondirectory a) a)) (directory-files cc-playground-basedir t "^[^.]"))
            :action (lambda (c) (find-file (concat (cdr c) "/snippet.cpp")))))

(defun cc-playground-copy ()
  "Copy a playground to a newly generated folder."
  (interactive)
  (if (cc-playground-inside)
      (let* ((snippet-file-name (cc-playground-snippet-file-name))
             (dst-dir (file-name-directory snippet-file-name))
             (snippet "snippet.cpp")
             (dirlocal ".dir-locals.el")
             (envrc ".envrc")
             (cquery ".cquery")
             (makefile "Makefile")
             (mainfile "main.cpp")
             (benchfile "bench.cpp")
             (testfile "test.cpp"))
        (copy-file snippet dst-dir)
        (copy-file envrc dst-dir)
        (copy-file cquery dst-dir)
        (copy-file dirlocal dst-dir)
        (copy-file makefile dst-dir)
        (copy-file mainfile dst-dir)
        (copy-file benchfile dst-dir)
        (copy-file testfile dst-dir)
        (find-file snippet-file-name)
        (my-reload-dir-locals-for-all-buffer-in-this-directory)
        (run-hooks 'cc-playground-hook))))

(defconst cc-playground--loaddir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that cc-playground was loaded from.")

;;;###autoload
(defun cc-playground ()
  "Run playground for C++ language in a new buffer."
  (interactive)
  (let ((snippet-file-name (cc-playground-snippet-file-name)))
    (switch-to-buffer (create-file-buffer snippet-file-name))
    (cc-playground-insert-template-head "snippet of code")
    (insert cc-template)
    (forward-line -2)
    (c++-mode)
    (cc-playground-mode)
    (set-visited-file-name snippet-file-name t)
    (let* ((dir-name (concat cc-playground--loaddir "templates/"))
           (dst-dir (file-name-directory snippet-file-name))
           (envrc (concat dir-name ".envrc"))
           (cquery (concat dir-name ".cquery"))
           (dirlocal (concat dir-name ".dir-locals.el"))
           (makefile (concat dir-name "Makefile"))
           (mainfile (concat dir-name "main.cpp"))
           (benchfile (concat dir-name "bench.cpp"))
           (testfile (concat dir-name "test.cpp")))
      (copy-file envrc dst-dir)
      (copy-file cquery dst-dir)
      (copy-file dirlocal dst-dir)
      (copy-file makefile dst-dir)
      (copy-file mainfile dst-dir)
      (copy-file benchfile dst-dir)
      (copy-file testfile dst-dir))
    (save-buffer)
    (my-reload-dir-locals-for-all-buffer-in-this-directory)
    (lsp-cquery-enable)
    (evil-open-below 1)
    (run-hooks 'cc-playground-hook)))

(defun cc-playground-insert-template-head (description)
  "Insert DESCRIPTION in the beginning of new snippets."
  (insert "// " description " @ " (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S") "

// === C++ Playground ===
// Execute the snippet with Ctrl-Return
// Remove the snippet completely with its dir and all files M-x `cc-playground-rm`

"))

(defun cc-playground-rm ()
  "Remove files of the current snippet together with directory of this snippet."
  (interactive)
  (if (cc-playground-inside)
      (if (or (not cc-playground-confirm-deletion)
              (y-or-n-p (format "Do you want delete whole snippet dir %s? "
                                (file-name-directory (buffer-file-name)))))
          (progn
            (run-hooks 'cc-playground-rm-hook)
            (save-buffer)
            (let ((dir (file-name-directory (buffer-file-name))))
              (delete-directory dir t t)
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (when (equal default-directory dir)
                    (let (kill-buffer-query-functions)
                      (kill-buffer buffer))))))))
    (message "Won't delete this! Because %s is not under the path %s. Remove the snippet manually!"
             (buffer-file-name) cc-playground-basedir)))

(defun cc-playground-snippet-unique-dir (prefix)
  "Get unique directory with PREFIX under `cc-playground-basedir`."
  (let ((dir-name (concat cc-playground-basedir "/"
                          (if (and prefix cc-playground-ask-file-name) (concat prefix "-"))
                          (time-stamp-string "default:%:y-%02m-%02d-%02H%02M%02S"))))
    (make-directory dir-name t)
    dir-name))

(defun cc-playground-inside ()
  "Is the current buffer is valid cc-playground buffer."
  (if (string-match-p (file-truename cc-playground-basedir) (file-truename (buffer-file-name)))
      (bound-and-true-p cc-playground-mode)))

(defun cc-playground-add-library-link (library)
  "Add an -llibrary line for LIBRARY near bottom of file, avoiding duplicates."
  (interactive "M#Library: ")
  (let ((lib (if (s-suffix? ".a" library)
                 (format "-l:%s \\" library)
               (format "-l%s \\" library))))
    (with-current-buffer (find-file-noselect (concat default-directory ".dir-locals.el"))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^-l.*\\\\$" nil 'stop-at-the-end 1)
        (end-of-line)
        (newline)
        (insert lib)
        (save-buffer)))))

(defun cc-playground-ivy-add-library-link ()
  "Add an -llibrary line using ivy-read."
  (interactive)
  (ivy-read "Library: " (nconc
                         (split-string
                          (shell-command-to-string "ldconfig -p | awk ' $1 ~ /^lib.*so$/ { print gensub(/^lib(.*).so$/, \"\\\\1\", 1, $1)}'"))
                         (split-string
                          (shell-command-to-string "g++ -print-search-dirs | sed -n 's/:/ /g;s/libraries.*=//p' | xargs ls | egrep '\.a$'")))
            :action #'cc-playground-add-library-link))

(defun cc-playground-switch-optimization-flag (flag)
  "Switch optimization flag to FLAG."
  (interactive "c#flags: ")
  (if (memq flag (list ?0 ?1 ?2 ?3 ?g))
      (let ((flags (format "-O%c \\" flag)))
        (with-current-buffer (find-file-noselect (concat default-directory ".dir-locals.el"))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "(cc-flags . \"" nil 'stop-at-the-end 1)
            (end-of-line)
            (if (re-search-forward "^-O.*$" nil t)
                (replace-match (concat flags "\\") nil nil)
              (newline)
              (insert flags))
            (let ((inhibit-message t))
              (save-buffer))
            (message "using optimization flag %c" flag))))
    (user-error (format "unknow optimization flag %c. known flags: 0, 1, 2, 3, g." flag))))

(defun cc-playground-add-compilation-flags (flags)
  "Add compilation flags FLAGS."
  (interactive "M#flags: ")
  (let ((flags (format "%s \\" flags)))
    (with-current-buffer (find-file-noselect (concat default-directory ".dir-locals.el"))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "(cc-flags . \"" nil 'stop-at-the-end 1)
        (end-of-line)
        (newline)
        (insert flags)
        (let ((inhibit-message t))
          (save-buffer))))))

(defun cc-playground-change-compiler ()
  "Change the compiler."
  (interactive)
  (let ((buffer (find-file-noselect (concat default-directory ".dir-locals.el"))))
    (+popup-buffer buffer '((window-parameters (select . t))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "(cc-exec . \"" nil 'stop-at-the-end 1)
      (evil-insert 1))))

(provide 'cc-playground)

;;; cc-playground.el ends here

