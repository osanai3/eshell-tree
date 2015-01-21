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

(defun eshell-tree-show-directory (directory prefix)
  (let ((self (concat prefix directory "\n"))
        (children (cl-remove-if-not
                   (symbol-function 'eshell-tree-displayable-file)
                   (directory-files-and-attributes directory))))
    (concat self "")))

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
