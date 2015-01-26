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

;; TODO:
;; close directory by click
;; open directory by click
;; max-depth
;; follow symlink

;;; Code:

(require 'cl-lib)

(defun eshell-tree (&rest filenames)
  (if filenames
      (mapconcat (symbol-function 'eshell-tree-single-file) filenames "")
    (eshell-tree-single-file ".")))

(defun eshell-tree-single-file (filename)
  (let ((attr (file-attributes filename)))
    (when attr
      (eshell-tree-show-file (cons filename attr) "" ""))))

(defun eshell-tree-file-button (filename)
  (with-output-to-string
    (with-current-buffer
        standard-output
      (insert-text-button
       filename
       'eshell-tree-file-name
       (concat default-directory filename)
       'action
       (lambda (button)
         (find-file (button-get button 'eshell-tree-file-name)))))))

(defun eshell-tree-directory-files (dirname)
  (cl-remove-if-not
   (symbol-function 'eshell-tree-displayable-file)
   (directory-files-and-attributes dirname)))

(defun eshell-tree-single-file-2 (filename)
  (let ((attr (file-attributes filename)))
    (when attr
      (eshell-tree-show-file-from-struct
       (eshell-tree-build-struct (cons filename attr)) "" ""))))

(defun eshell-tree-make-struct (file open children)
  (list (cons 'file file) (cons 'open open) (cons 'children children)))

(defun eshell-tree-build-struct (file)
  (cond
   ((stringp (cadr file)) (eshell-tree-make-struct file nil nil)) ;; symlink
   ((cadr file)
    (eshell-tree-make-struct
     file t
     (let ((children (eshell-tree-directory-files (car file)))
           (default-directory (concat default-directory (car file) "/")))
       (mapcar 'eshell-tree-build-struct children)))) ;; directory
   (t (eshell-tree-make-struct file nil nil)) ;; regular file
   ))

(defun eshell-tree-show-file-from-struct (struct prefix-self prefix-child)
  (let ((file (cdr (assoc 'file struct)))
        (open (cdr (assoc 'open struct)))
        (children (cdr (assoc 'children struct))))
    (concat
     prefix-self (eshell-tree-file-button (car file))
     (when (cadr file) (concat " " "[-]"))
     "\n"
     (cond
      ((stringp (cadr file)) "") ;;symlink
      ((cadr file)
       (let ((default-directory (car file))
             (preceding (reverse (cdr (reverse children))))
             (last (car (reverse children))))
         (concat
          (apply 'concat
                 (mapcar (lambda (file)
                           (eshell-tree-show-file-from-struct
                            file
                            (concat prefix-child "|-- ")
                            (concat prefix-child "|   ")))
                         preceding))
          (eshell-tree-show-file-from-struct
           last (concat prefix-child "`-- ") (concat prefix-child "    "))))) ;; directory
      (t "") ;; regular file
      ))))

(defun eshell-tree-show-file (file prefix-self prefix-child)
  (concat
   prefix-self (eshell-tree-file-button (car file))
   (when (cadr file) (concat " " "[-]"))
   "\n"
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
