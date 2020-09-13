;;; magit-files.el --- finding files  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2020  The Magit Project Contributors
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

;; This library implements support for finding blobs, staged files,
;; and Git configuration files.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'magit)

(declare-function magit-blob-mode "magit-file-modes")

;;; Find Blob

(defvar magit-find-file-hook nil)
(add-hook 'magit-find-file-hook #'magit-blob-mode)

;;;###autoload
(defun magit-find-file (rev file)
  "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file"))
  (magit-find-file--internal rev file #'pop-to-buffer-same-window))

;;;###autoload
(defun magit-find-file-other-window (rev file)
  "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file in other window"))
  (magit-find-file--internal rev file #'switch-to-buffer-other-window))

;;;###autoload
(defun magit-find-file-other-frame (rev file)
  "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file in other frame"))
  (magit-find-file--internal rev file #'switch-to-buffer-other-frame))

(defun magit-find-file-read-args (prompt)
  (let ((pseudo-revs '("{worktree}" "{index}")))
    (if-let ((rev (magit-completing-read "Find file from revision"
                                         (append pseudo-revs
                                                 (magit-list-refnames nil t))
                                         nil nil nil 'magit-revision-history
                                         (or (magit-branch-or-commit-at-point)
                                             (magit-get-current-branch)))))
        (list rev (magit-read-file-from-rev (if (member rev pseudo-revs)
                                                "HEAD"
                                              rev)
                                            prompt))
      (user-error "Nothing selected"))))

(defun magit-find-file--internal (rev file fn)
  (let ((buf (magit-find-file-noselect rev file))
        line col)
    (when-let ((visited-file (magit-file-relative-name)))
      (setq line (line-number-at-pos))
      (setq col (current-column))
      (cond
       ((not (equal visited-file file)))
       ((equal magit-buffer-revision rev))
       ((equal rev "{worktree}")
        (setq line (magit-diff-visit--offset file magit-buffer-revision line)))
       ((equal rev "{index}")
        (setq line (magit-diff-visit--offset file nil line)))
       (magit-buffer-revision
        (setq line (magit-diff-visit--offset
                    file (concat magit-buffer-revision ".." rev) line)))
       (t
        (setq line (magit-diff-visit--offset file (list "-R" rev) line)))))
    (funcall fn buf)
    (when line
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)))
    buf))

(defun magit-find-file-noselect (rev file)
  "Read FILE from REV into a buffer and return the buffer.
REV is a revision or one of \"{worktree}\" or \"{index}\".
FILE must be relative to the top directory of the repository."
  (magit-find-file-noselect-1 rev file))

(defun magit-find-file-noselect-1 (rev file &optional revert)
  "Read FILE from REV into a buffer and return the buffer.
REV is a revision or one of \"{worktree}\" or \"{index}\".
FILE must be relative to the top directory of the repository.
Non-nil REVERT means to revert the buffer.  If `ask-revert',
then only after asking.  A non-nil value for REVERT is ignored if REV is
\"{worktree}\"."
  (if (equal rev "{worktree}")
      (find-file-noselect (expand-file-name file (magit-toplevel)))
    (let ((topdir (magit-toplevel)))
      (when (file-name-absolute-p file)
        (setq file (file-relative-name file topdir)))
      (with-current-buffer (magit-get-revision-buffer-create rev file)
        (when (or (not magit-buffer-file-name)
                  (if (eq revert 'ask-revert)
                      (y-or-n-p (format "%s already exists; revert it? "
                                        (buffer-name))))
                  revert)
          (setq magit-buffer-revision
                (if (equal rev "{index}")
                    "{index}"
                  (magit-rev-format "%H" rev)))
          (setq magit-buffer-refname rev)
          (setq magit-buffer-file-name (expand-file-name file topdir))
          (setq default-directory
                (let ((dir (file-name-directory magit-buffer-file-name)))
                  (if (file-exists-p dir) dir topdir)))
          (setq-local revert-buffer-function #'magit-revert-rev-file-buffer)
          (revert-buffer t t)
          (run-hooks (if (equal rev "{index}")
                         'magit-find-index-hook
                       'magit-find-file-hook)))
        (current-buffer)))))

(defun magit-get-revision-buffer-create (rev file)
  (magit-get-revision-buffer rev file t))

(defun magit-get-revision-buffer (rev file &optional create)
  (funcall (if create 'get-buffer-create 'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun magit-revert-rev-file-buffer (_ignore-auto noconfirm)
  (when (or noconfirm
            (and (not (buffer-modified-p))
                 (catch 'found
                   (dolist (regexp revert-without-query)
                     (when (string-match regexp magit-buffer-file-name)
                       (throw 'found t)))))
            (yes-or-no-p (format "Revert buffer from Git %s? "
                                 (if (equal magit-buffer-refname "{index}")
                                     "index"
                                   (concat "revision " magit-buffer-refname)))))
    (let* ((inhibit-read-only t)
           (default-directory (magit-toplevel))
           (file (file-relative-name magit-buffer-file-name))
           (coding-system-for-read (or coding-system-for-read 'undecided)))
      (erase-buffer)
      (magit-git-insert "cat-file" "-p"
                        (if (equal magit-buffer-refname "{index}")
                            (concat ":" file)
                          (concat magit-buffer-refname ":" file)))
      (setq buffer-file-coding-system last-coding-system-used))
    (let ((buffer-file-name magit-buffer-file-name)
          (after-change-major-mode-hook
           (remq 'global-diff-hl-mode-enable-in-buffers
                 after-change-major-mode-hook)))
      (normal-mode t))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

;;; Find Index

(defvar magit-find-index-hook nil)

(defun magit-find-file-index-noselect (file &optional revert)
  "Read FILE from the index into a buffer and return the buffer.
FILE must to be relative to the top directory of the repository."
  (magit-find-file-noselect-1 "{index}" file (or revert 'ask-revert)))

(defun magit-update-index ()
  "Update the index with the contents of the current buffer.
The current buffer has to be visiting a file in the index, which
is done using `magit-find-index-noselect'."
  (interactive)
  (let ((file (magit-file-relative-name)))
    (unless (equal magit-buffer-refname "{index}")
      (user-error "%s isn't visiting the index" file))
    (if (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
        (let ((index (make-temp-file "index"))
              (buffer (current-buffer)))
          (when magit-wip-before-change-mode
            (magit-wip-commit-before-change (list file) " before un-/stage"))
          (let ((coding-system-for-write buffer-file-coding-system))
            (with-temp-file index
              (insert-buffer-substring buffer)))
          (magit-with-toplevel
            (magit-call-git "update-index" "--cacheinfo"
                            (substring (magit-git-string "ls-files" "-s" file)
                                       0 6)
                            (magit-git-string "hash-object" "-t" "blob" "-w"
                                              (concat "--path=" file)
                                              "--" index)
                            file))
          (set-buffer-modified-p nil)
          (when magit-wip-after-apply-mode
            (magit-wip-commit-after-apply (list file) " after un-/stage")))
      (message "Abort")))
  (--when-let (magit-get-mode-buffer 'magit-status-mode)
    (with-current-buffer it (magit-refresh)))
  t)

;;; Find Config File

(defun magit-find-git-config-file (filename &optional wildcards)
  "Edit a file located in the current repository's git directory.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file', except that it temporarily
binds `default-directory' to the actual git directory, while
reading the FILENAME."
  (interactive
   (let ((default-directory (magit-git-dir)))
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file filename wildcards))

(defun magit-find-git-config-file-other-window (filename &optional wildcards)
  "Edit a file located in the current repository's git directory, in another window.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-window', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
   (let ((default-directory (magit-git-dir)))
     (find-file-read-args "Find file in other window: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file-other-window filename wildcards))

(defun magit-find-git-config-file-other-frame (filename &optional wildcards)
  "Edit a file located in the current repository's git directory, in another frame.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-frame', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
   (let ((default-directory (magit-git-dir)))
     (find-file-read-args "Find file in other frame: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file-other-frame filename wildcards))

;;; Read File

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (rev prompt &optional default)
  (let ((files (magit-revision-files rev)))
    (magit-completing-read
     prompt files nil t nil 'magit-read-file-hist
     (car (member (or default (magit-current-file)) files)))))

(defun magit-read-file (prompt &optional tracked-only)
  (let ((choices (nconc (magit-list-files)
                        (unless tracked-only (magit-untracked-files)))))
    (magit-completing-read
     prompt choices nil t nil nil
     (car (member (or (magit-section-value-if '(file submodule))
                      (magit-file-relative-name nil tracked-only))
                  choices)))))

(defun magit-read-tracked-file (prompt)
  (magit-read-file prompt t))

(defun magit-read-file-choice (prompt files &optional error default)
  "Read file from FILES.

If FILES has only one member, return that instead of prompting.
If FILES has no members, give a user error.  ERROR can be given
to provide a more informative error.

If DEFAULT is non-nil, use this as the default value instead of
`magit-current-file'."
  (pcase (length files)
    (0 (user-error (or error "No file choices")))
    (1 (car files))
    (_ (magit-completing-read
        prompt files nil t nil 'magit-read-file-hist
        (car (member (or default (magit-current-file)) files))))))

(defun magit-read-changed-file (rev-or-range prompt &optional default)
  (magit-read-file-choice
   prompt
   (magit-changed-files rev-or-range)
   default
   (concat "No file changed in " rev-or-range)))

;;; _
(provide 'magit-files)
;;; magit-files.el ends here
