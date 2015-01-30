;;; eshell-tree.el --- show directory tree -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Koichi Osanai

;; Author: Koichi Osanai <osanai3@gmail.com>
;; Version: 0.1.0

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

(defun eshell-tree (&rest filenames)
  (if filenames
      (mapconcat (symbol-function 'eshell-tree-single-file) filenames "")
    (eshell-tree-single-file ".")))

(defun eshell-tree-single-file (filename)
  (let ((struct (eshell-tree-build-struct-from-filename filename "")))
    (when struct
      (eshell-tree-show-file-from-struct struct))))

(defun eshell-tree-directory-files (dirname)
  (cl-remove-if-not
   (symbol-function 'eshell-tree-displayable-file)
   (directory-files-and-attributes dirname)))

(defun eshell-tree-build-struct-from-filename (filename directory)
  (let ((attr (file-attributes filename)))
    (when attr
      (eshell-tree-build-struct (cons filename attr) directory))))

(defun eshell-tree-build-struct (file directory)
  (cons
   (list
    (cons 'file file)
    (cons 'directory directory))
   (when (eq t (cadr file)) ; directory
     (mapcar
      (lambda (child)
        (eshell-tree-build-struct child (concat directory (car file) "/")))
      (eshell-tree-directory-files (concat directory (car file)))))))

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

(defun eshell-tree-file-button (filename directory)
  (with-output-to-string
    (with-current-buffer
        standard-output
      (insert-text-button
       filename
       'eshell-tree-file-name
       (concat default-directory directory filename)
       'action
       (lambda (button)
         (find-file (button-get button 'eshell-tree-file-name)))))))

(defun eshell-tree-show-single-file-from-struct (struct)
  (let ((file (cdr (assoc 'file struct)))
        (directory (cdr (assoc 'directory struct))))
    (concat (eshell-tree-file-button (car file) directory) "\n")))

(defun eshell-tree-show-file-from-struct (struct)
  (eshell-tree-treeize-struct
   (eshell-tree-map-struct 'eshell-tree-show-single-file-from-struct struct)))

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
