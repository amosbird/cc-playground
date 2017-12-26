;;; cc-playground.el --- Local C/C++ playground for short snippets.

;; Copyright (C) 2017 Amos Bird

;; Author: Amos Bird <amosbird@gmail.com>
;; URL: https://github.com/amosbird/cc-playground
;; Keywords: tools, c/c++
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

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
  "Non-nil means you will be asked for confirmation on the snippet deletion with `cc-playground-rm [S-return]'.

By default confirmation required."
  :type 'boolean
  :group 'cc-playground)

(defcustom cc-playground-basedir "~/cc/playground"
  "Base directory for playground snippets."
  :type 'file
  :group 'cc-playground)

(define-minor-mode cc-playground-mode
  "A place for playing with c++ code."
  :init-value nil
  :lighter "Play(C/C++)"
  :keymap '(([C-return] . cc-playground-exec)
            ([S-return] . cc-playground-rm)))

(defun cc-playground-snippet-file-name(&optional snippet-name)
  (let ((file-name (cond (snippet-name)
                         (cc-playground-ask-file-name
                          (read-string "C++ Playground filename: "))
                         ("snippet"))))
    (concat (cc-playground-snippet-unique-dir file-name) "/" file-name ".cpp")))

;
(defun cc-playground-save-and-run ()
  "Obsoleted by cc-playground-exec."
  (interactive)
  (cc-playground-exec))

(defun cc-playground-exec ()
  "Save the buffer then runs clang compiler for executing the code."
  (interactive)
  (if (cc-playground-inside)
	  (progn
		(save-buffer t)
		(make-local-variable 'compile-command)
		(compile "clang++ -std=c++17 *.cpp && ./a.out"))))

;;;###autoload
(defun cc-playground ()
  "Run playground for C++ language in a new buffer."
  (interactive)
  (let ((snippet-file-name (cc-playground-snippet-file-name)))
    (switch-to-buffer (create-file-buffer snippet-file-name))
	(cc-playground-insert-template-head "snippet of code")
(insert "#include <iostream>

using namespace std;

int main() {
	cout << \"Result: \" << endl;
}
")
    (backward-char 3)
    (c++-mode)
    (cc-playground-mode)
    (set-visited-file-name snippet-file-name t)))

(defun cc-playground-insert-template-head (description)
  (insert "// -*- mode:c++;mode:cc-playground -*-
// " description " @ " (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S") "

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
			(save-buffer)
			(delete-directory (file-name-directory (buffer-file-name)) t t)
			(kill-buffer)))
	(message "Won't delete this! Because %s is not under the path %s. Remove the snippet manually!"
			 (buffer-file-name) cc-playground-basedir)))

;;;###autoload
(defun cc-playground-remove-current-snippet ()
    "Obsoleted by `cc-playground-rm'."
  (interactive)
  (cc-playground-rm))

(defun cc-playground-snippet-unique-dir (prefix)
  "Get unique directory under `cc-playground-basedir`."
  (let ((dir-name (concat cc-playground-basedir "/"
                          (if (and prefix cc-playground-ask-file-name) (concat prefix "-"))
                          (time-stamp-string "at-%:y-%02m-%02d-%02H%02M%02S"))))
    (make-directory dir-name t)
    dir-name))

(defun cc-playground-inside ()
  "Is the current buffer is valid cc-playground buffer."
  (if (string-match-p (file-truename cc-playground-basedir) (file-truename (buffer-file-name)))
	  (bound-and-true-p cc-playground-mode)))

(provide 'cc-playground)
;;; cc-playground.el ends here
