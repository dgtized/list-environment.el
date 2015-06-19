;;; list-environment.el --- A tabulated process environment editor  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Charles L.G. Comstock

;; Author: Charles L.G. Comstock <dgtized@gmail.com>
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

(defun list-environment ()
  (interactive)
  (let ((buffer (get-buffer-create "*Environment*")))
    (set-buffer buffer)
    (dolist (env process-environment)
      (insert env "\n"))
    (pop-to-buffer buffer)))

(provide 'list-environment)
;;; list-environment.el ends here
