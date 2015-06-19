;;; list-environment.el --- A tabulated process environment editor  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Charles L.G. Comstock

;; Author: Charles L.G. Comstock <dgtized@gmail.com>
;; Packages-Requires: ((emacs "24.1"))
;; Keywords: processes, unix

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

;; Tabulated environment listing the process environment

;;; Code:

(require 'tabulated-list)

(defun list-environment-entries ()
  (mapcar (lambda (env)
            (let ((keyvals (split-string env "=")))
              (list (car keyvals) (vconcat keyvals))))
          process-environment))

(define-derived-mode list-environment-mode tabulated-list-mode "Process Environment"
  "Major mode for listing process environment"
  (setq tabulated-list-format [("NAME" 20 t)
                               ("VALUE" 50 t)]
        tabulated-list-sort-key (cons "NAME" nil)
        tabulated-list-padding 2
        tabulated-list-entries #'list-environment-entries)
  (tabulated-list-init-header))

(defun list-environment ()
  "List process environment in a tabulated view"
  (interactive)
  (let ((buffer (get-buffer-create "*Process-Environment*")))
    (pop-to-buffer buffer)
    (list-environment-mode)
    (tabulated-list-print)))

(provide 'list-environment)
;;; list-environment.el ends here
