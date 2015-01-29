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

(defun eshell-tree-single-file (filename)
  (let ((struct (eshell-tree-build-struct-from-filename filename)))
    (when struct
      (eshell-tree-show-file-from-struct struct "" ""))))

(defun eshell-tree-directory-files (dirname)
  (cl-remove-if-not
   (symbol-function 'eshell-tree-displayable-file)
   (directory-files-and-attributes dirname)))

(defun eshell-tree-build-struct-from-filename (filename)
  (let ((attr (file-attributes filename)))
    (when attr
      (eshell-tree-build-struct (cons filename attr)))))

(defun eshell-tree-build-struct (file)
  (cons (list (cons 'file file) (cons 'open nil)) nil))

(defun eshell-tree-fold-struct (func struct)
  (funcall
   func
   (car struct)
   (mapcar (apply-partially 'eshell-tree-fold-struct func) (cdr struct))))

(defun eshell-tree-map-struct (func struct)
  (cons
   (funcall func (car struct))
   (mapcar (apply-partially 'eshell-tree-map-struct func) (cdr struct))))

(defconst eshell-tree-test-struct
  (cons "aa\n"
        (list
         (cons "bbb\n"
               (list (cons "ccc\n" nil)))
         (cons "dddd\n"
               (list (cons "eeee\n" nil))))))

(cl-assert
 (equal
  (eshell-tree-fold-struct
   (lambda (self children) (apply 'concat self children))
   eshell-tree-test-struct)
  "aa\nbbb\nccc\ndddd\neeee\n"))

(cl-assert
 (equal
  (eshell-tree-map-struct
   (lambda (x) (length x))
   eshell-tree-test-struct)
  '(3 (4 (4)) (5 (5)))))

(defun eshell-tree-suffixp (suffix string)
  (equal suffix (substring string (- (min (length string) (length suffix))))))

(defun eshell-tree-lines (string)
  (cond ((equal string "") nil)
        ((eshell-tree-suffixp "\n" string) (split-string (substring string 0 -1) "\n"))
        (t (split-string string "\n"))))

(defun eshell-tree-unlines (strings)
  (mapconcat (lambda (line) (concat line "\n")) strings ""))

(defun eshell-tree-add-line-prefix (first-prefix rest-prefix string)
  (let ((lines (eshell-tree-lines string)))
    (eshell-tree-unlines
     (cons
      (concat first-prefix (car lines))
      (mapcar (apply-partially 'concat rest-prefix) (cdr lines))
      ))))

(cl-assert
 (equal
  (eshell-tree-add-line-prefix "a" "b" "c\nc\nc\n")
  "ac\nbc\nbc\n"))

(defun eshell-tree-treeize-struct (struct)
  (eshell-tree-fold-struct
   (lambda (self children)
     (let ((preceding (reverse (cdr (reverse children))))
           (last (car (reverse children))))
       (concat
        self
        (apply 'concat (mapcar
         (apply-partially 'eshell-tree-add-line-prefix "|-- " "|   ")
         preceding))
        (when last (eshell-tree-add-line-prefix "`-- " "    " last)))
       ))
   struct))

(cl-assert
 (equal
  (eshell-tree-treeize-struct eshell-tree-test-struct)
  "aa\n|-- bbb\n|   `-- ccc\n`-- dddd\n    `-- eeee\n"))

(defun eshell-tree-map-fold-struct (map fold struct)
  (eshell-tree-fold-struct fold (eshell-tree-map-struct map struct)))

;(defun eshell-tree-show-file-from-struct (struct prefix-self prefix-child)
;  (let ((file (cdr (assoc 'file struct)))
;        (open (cdr (assoc 'open struct)))
;        (children (cdr (assoc 'children struct))))
;    (concat
;     prefix-self (eshell-tree-file-button (car file))
;     (when (cadr file) (concat " " "[-]"))
;     "\n"
;     (cond
;      ((stringp (cadr file)) "") ;;symlink
;      ((cadr file)
;       (let ((default-directory (concat default-directory (car file) "/"))
;             (preceding (reverse (cdr (reverse children))))
;             (last (car (reverse children))))
;         (concat
;          (apply 'concat
;                 (mapcar (lambda (file)
;                           (eshell-tree-show-file-from-struct
;                            file
;                            (concat prefix-child "|-- ")
;                            (concat prefix-child "|   ")))
;                         preceding))
;          (eshell-tree-show-file-from-struct
;           last (concat prefix-child "`-- ") (concat prefix-child "    "))))) ;; directory
;      (t "") ;; regular file
;      ))))

(defun eshell-tree-count-lines (struct)
  (eshell-tree-map-fold-struct
   (lambda (struct) 1)
   (lambda (self children) (apply '+ self children))
   struct))

(cl-assert
 (equal
  5
  (eshell-tree-count-lines eshell-tree-test-struct)))

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
