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

(defvar list-environment-name-history nil)
(defvar list-environment-value-history nil)

(defun list-environment-addenv (name value)
  (interactive
   (let ((n (read-string "Name: " nil 'list-environment-name-history)))
     (list n (read-string (format "Setenv %s: " n)
                          nil
                          'list-environment-value-history))))
  (message "Setenv %s: %s" name value)
  (setenv name value)
  (tabulated-list-revert))

(defun list-environment-setenv (value)
  (interactive
   (list
    (let ((entry (tabulated-list-get-entry)))
      (read-string (format "Setenv %s: " (aref entry 0))
                   (aref entry 1)
                   'list-environment-value-history))))
  (let ((entry (tabulated-list-get-entry)))
    (message "Setenv %s %s -> %s" (aref entry 0) (aref entry 1) value)
    (setenv (aref entry 0) value)
    (tabulated-list-revert)))

(defun list-environment-entries ()
  (mapcar (lambda (env)
            (let* ((kv (split-string env "="))
                   (keyvals (if (= (length kv) 2)
                                kv
                              (append kv (list "")))))
              (list (car keyvals) (vconcat keyvals))))
          process-environment))

(define-derived-mode list-environment-mode
    tabulated-list-mode "Process-Environment"
  "Major mode for listing process environment.
\\{list-environment-mode-map\}"
  (setq tabulated-list-format [("NAME" 20 t)
                               ("VALUE" 50 t)]
        tabulated-list-sort-key (cons "NAME" nil)
        tabulated-list-padding 2
        tabulated-list-entries #'list-environment-entries)
  (tabulated-list-init-header))

(define-key list-environment-mode-map (kbd "s") 'list-environment-setenv)
(define-key list-environment-mode-map (kbd "a") 'list-environment-addenv)

(defun list-environment ()
  "List process environment in a tabulated view"
  (interactive)
  (let ((buffer (get-buffer-create "*Process-Environment*")))
    (pop-to-buffer buffer)
    (list-environment-mode)
    (tabulated-list-print)))

(provide 'list-environment)
;;; list-environment.el ends here
