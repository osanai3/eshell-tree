;;; eshell-tree.el --- show directory tree -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Koichi Osanai

;; Author: Koichi Osanai <osanai3@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Usage:
;; (require 'eshell-tree)
;; (fset 'eshell/tree (symbol-function 'eshell-tree))

;;; Code:

(require 'cl-lib)

(defun eshell-tree (filename)
  (eshell-tree-show-file (cons filename (file-attributes filename)) "" ""))

(defun eshell-tree-show-file (file prefix-self prefix-child)
  (concat
   prefix-self (car file) "\n"
   (cond
    ((stringp (cadr file)) "") ;; symlink
    ((cadr file) (eshell-tree-show-directory-content
                 (car file) prefix-child)) ;; directory
    (t "") ;; regular file
    )))

(defun eshell-tree-show-directory-content (dirname prefix)
  (let ((children (cl-remove-if-not
                   (symbol-function 'eshell-tree-displayable-file)
                   (directory-files-and-attributes dirname))))
    (when children
      (let ((preceding (reverse (cdr (reverse children))))
            (last (car (reverse children)))
            (default-directory (concat default-directory dirname "/")))
        (concat
         (apply 'concat
                (mapcar
                 (lambda (file)
                   (eshell-tree-show-file
                    file
                    (concat prefix "|-- ") (concat prefix "|   ")))
                 preceding))
         (eshell-tree-show-file
          last
          (concat prefix "`-- ") (concat prefix "    ")))))))

(defun eshell-tree-add-directory (dirname file)
  (cons (concat dirname "/" (car file)) (cdr file)))

(defun eshell-tree-displayable-file (file)
  (eshell-tree-displayable-filename (nth 0 file)))

(defun eshell-tree-displayable-filename (filename)
  (not (equal (substring filename 0 1) ".")))

(cl-assert
 (eshell-tree-displayable-filename "aaa.txt"))

(cl-assert
 (not (eshell-tree-displayable-filename "..")))

(provide 'eshell-tree)

;;; eshell-tree.el ends here
