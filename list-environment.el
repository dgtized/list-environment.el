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

(defun list-environment-addenv ()
  "Set a new environment variable."
  (interactive)
  (call-interactively 'setenv)
  (tabulated-list-revert))

(defun list-environment-clear ()
  "Remove current environment variable value."
  (interactive)
  (let ((current-prefix-arg t))
    (list-environment-setenv)))

(defun list-environment-setenv ()
  "Edit the value of current environment variable."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (minibuffer-with-setup-hook
        (lambda () (insert name))
        (call-interactively 'setenv))
    (tabulated-list-revert)))

(defun list-environment-entries ()
  "Generate environment variable entries list for tabulated-list."
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
(define-key list-environment-mode-map (kbd "d") 'list-environment-clear)

(defun list-environment ()
  "List process environment in a tabulated view."
  (interactive)
  (let ((buffer (get-buffer-create "*Process-Environment*")))
    (pop-to-buffer buffer)
    (list-environment-mode)
    (tabulated-list-print)))

(provide 'list-environment)
;;; list-environment.el ends here
