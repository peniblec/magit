;;; magit-file-modes.el --- commands for files and blobs  -*- lexical-binding: t -*-

;; Copyright (C) 2020  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library implements modes useful in buffers visiting files and
;; blobs, and the commands used by those modes.

;;; Code:

;;; File Mode

(eval-when-compile
  (require 'dash)
  (require 'subr-x))

(require 'transient)

(defvar magit-buffer-file-name)
(defvar magit-buffer-revision)

(require 'magit-git)
(require 'magit-process)

(declare-function magit-find-file "magit-files")
(declare-function magit-find-file--internal "magit-files")
(declare-function magit-read-file "magit-files")
(declare-function magit-read-file-from-rev "magit-files")
(declare-function magit-read-tracked-file "magit-files")

(declare-function magit--age "magit-margin")

(declare-function magit-refresh "magit-mode")

(defvar magit-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-xg"    'magit-status)
    (define-key map "\C-x\M-g" 'magit-dispatch)
    (define-key map "\C-c\M-g" 'magit-file-dispatch)
    map)
  "Keymap for `magit-file-mode'.")

;;;###autoload (autoload 'magit-file-dispatch "magit" nil t)
(transient-define-prefix magit-file-dispatch ()
  "Invoke a Magit command that acts on the visited file."
  :info-manual "(magit) Minor Mode for Buffers Visiting Files"
  ["Actions"
   [("s" "Stage"      magit-stage-file)
    ("u" "Unstage"    magit-unstage-file)
    ("c" "Commit"     magit-commit)
    ("e" "Edit line"  magit-edit-line-commit)]
   [("D" "Diff..."    magit-diff)
    ("d" "Diff"       magit-diff-buffer-file)
    ("g" "Status"     magit-status-here)]
   [("L" "Log..."     magit-log)
    ("l" "Log"        magit-log-buffer-file)
    ("t" "Trace"      magit-log-trace-definition)]
   [("B" "Blame..."   magit-blame)
    ("b" "Blame"      magit-blame-addition)
    ("r" "...removal" magit-blame-removal)
    ("f" "...reverse" magit-blame-reverse)
    ("m" "Blame echo" magit-blame-echo)
    ("q" "Quit blame" magit-blame-quit)]
   [("p" "Prev blob"  magit-blob-previous)
    ("n" "Next blob"  magit-blob-next)
    ("v" "Goto blob"  magit-find-file)
    ("V" "Goto file"  magit-blob-visit-file)]
   [(5 "C-c r" "Rename file"   magit-file-rename)
    (5 "C-c d" "Delete file"   magit-file-delete)
    (5 "C-c u" "Untrack file"  magit-file-untrack)
    (5 "C-c c" "Checkout file" magit-file-checkout)]])

(defvar magit-file-mode-lighter "")

(define-minor-mode magit-file-mode
  "Enable some Magit features in a file-visiting buffer.

Currently this only adds the following key bindings.
\n\\{magit-file-mode-map}"
  :package-version '(magit . "2.2.0")
  :lighter magit-file-mode-lighter
  :keymap  magit-file-mode-map)

(defun magit-file-mode-turn-on ()
  (and buffer-file-name
       (magit-inside-worktree-p t)
       (magit-file-mode)))

;;;###autoload
(define-globalized-minor-mode global-magit-file-mode
  magit-file-mode magit-file-mode-turn-on
  :package-version '(magit . "2.13.0")
  :link '(info-link "(magit)Minor Mode for Buffers Visiting Files")
  :group 'magit-essentials
  :group 'magit-modes
  :init-value t)
;; Unfortunately `:init-value t' only sets the value of the mode
;; variable but does not cause the mode function to be called, and we
;; cannot use `:initialize' to call that explicitly because the option
;; is defined before the functions, so we have to do it here.
(cl-eval-when (load eval)
  (when global-magit-file-mode
    (global-magit-file-mode 1)))

;;; Blob Mode

(defvar magit-blob-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'magit-blob-previous)
    (define-key map "n" 'magit-blob-next)
    (define-key map "b" 'magit-blame-addition)
    (define-key map "r" 'magit-blame-removal)
    (define-key map "f" 'magit-blame-reverse)
    (define-key map "q" 'magit-kill-this-buffer)
    map)
  "Keymap for `magit-blob-mode'.")

(define-minor-mode magit-blob-mode
  "Enable some Magit features in blob-visiting buffers.

Currently this only adds the following key bindings.
\n\\{magit-blob-mode-map}"
  :package-version '(magit . "2.3.0"))

(defun magit-blob-next ()
  "Visit the next blob which modified the current file."
  (interactive)
  (if magit-buffer-file-name
      (magit-blob-visit (or (magit-blob-successor magit-buffer-revision
                                                  magit-buffer-file-name)
                            magit-buffer-file-name))
    (if (buffer-file-name (buffer-base-buffer))
        (user-error "You have reached the end of time")
      (user-error "Buffer isn't visiting a file or blob"))))

(defun magit-blob-previous ()
  "Visit the previous blob which modified the current file."
  (interactive)
  (if-let ((file (or magit-buffer-file-name
                     (buffer-file-name (buffer-base-buffer)))))
      (--if-let (magit-blob-ancestor magit-buffer-revision file)
          (magit-blob-visit it)
        (user-error "You have reached the beginning of time"))
    (user-error "Buffer isn't visiting a file or blob")))

;;;###autoload
(defun magit-blob-visit-file ()
  "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree."
  (interactive)
  (if-let ((file (magit-file-relative-name)))
      (magit-find-file--internal "{worktree}" file #'pop-to-buffer-same-window)
    (user-error "Not visiting a blob")))

(defun magit-blob-visit (blob-or-file)
  (if (stringp blob-or-file)
      (find-file blob-or-file)
    (pcase-let ((`(,rev ,file) blob-or-file))
      (magit-find-file rev file)
      (apply #'message "%s (%s %s ago)"
             (magit-rev-format "%s" rev)
             (magit--age (magit-rev-format "%ct" rev))))))

(defun magit-blob-ancestor (rev file)
  (let ((lines (magit-with-toplevel
                 (magit-git-lines "log" "-2" "--format=%H" "--name-only"
                                  "--follow" (or rev "HEAD") "--" file))))
    (if rev (cddr lines) (butlast lines 2))))

(defun magit-blob-successor (rev file)
  (let ((lines (magit-with-toplevel
                 (magit-git-lines "log" "--format=%H" "--name-only" "--follow"
                                  "HEAD" "--" file))))
    (catch 'found
      (while lines
        (if (equal (nth 2 lines) rev)
            (throw 'found (list (nth 0 lines) (nth 1 lines)))
          (setq lines (nthcdr 2 lines)))))))

;;; File Commands

(defun magit-file-rename (file newname)
  "Rename the FILE to NEWNAME.
If FILE isn't tracked in Git, fallback to using `rename-file'."
  (interactive
   (let* ((file (magit-read-file "Rename file"))
          (dir (file-name-directory file))
          (newname (read-file-name (format "Rename %s to file: " file)
                                   (and dir (expand-file-name dir)))))
     (list (expand-file-name file (magit-toplevel))
           (expand-file-name newname))))
  (let ((oldbuf (get-file-buffer file)))
    (when (and oldbuf (buffer-modified-p oldbuf))
      (user-error "Save %s before moving it" file))
    (when (file-exists-p newname)
      (user-error "%s already exists" newname))
    (if (magit-file-tracked-p (magit-convert-filename-for-git file))
        (magit-call-git "mv"
                        (magit-convert-filename-for-git file)
                        (magit-convert-filename-for-git newname))
      (rename-file file newname current-prefix-arg))
    (when oldbuf
      (with-current-buffer oldbuf
        (let ((buffer-read-only buffer-read-only))
          (set-visited-file-name newname nil t))
        (if (fboundp 'vc-refresh-state)
            (vc-refresh-state)
          (with-no-warnings
            (vc-find-file-hook))))))
  (magit-refresh))

(defun magit-file-untrack (files &optional force)
  "Untrack the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
staged as well as unstaged changes."
  (interactive (list (or (--if-let (magit-region-values 'file t)
                             (progn
                               (unless (magit-file-tracked-p (car it))
                                 (user-error "Already untracked"))
                               (magit-confirm-files 'untrack it "Untrack"))
                           (list (magit-read-tracked-file "Untrack file"))))
                     current-prefix-arg))
  (magit-with-toplevel
    (magit-run-git "rm" "--cached" (and force "--force") "--" files)))

(defun magit-file-delete (files &optional force)
  "Delete the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
uncommitted changes.  When the files aren't being tracked in
Git, then fallback to using `delete-file'."
  (interactive (list (--if-let (magit-region-values 'file t)
                         (magit-confirm-files 'delete it "Delete")
                       (list (magit-read-file "Delete file")))
                     current-prefix-arg))
  (if (magit-file-tracked-p (car files))
      (magit-call-git "rm" (and force "--force") "--" files)
    (let ((topdir (magit-toplevel)))
      (dolist (file files)
        (delete-file (expand-file-name file topdir) t))))
  (magit-refresh))

;;;###autoload
(defun magit-file-checkout (rev file)
  "Checkout FILE from REV."
  (interactive
   (let ((rev (magit-read-branch-or-commit
               "Checkout from revision" magit-buffer-revision)))
     (list rev (magit-read-file-from-rev rev "Checkout file"))))
  (magit-with-toplevel
    (magit-run-git "checkout" rev "--" file)))

;;; _
(provide 'magit-file-modes)
;;; magit-file-modes.el ends here
