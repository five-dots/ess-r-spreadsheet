;;; ess-r-spreadsheet.el --- Open R objects in a spreadsheet software

;; Copyright (C) 2019 Shun Asai

;; Author: Shun Asai <syun.asai@gmail.com>
;; Maintainer: Shun Asai <syun.asai@gmail.com>
;; URL: https://github.com/five-dots/ess-r-spreadsheet
;; Created: 2019-10-04
;; Version: 0.1
;; Package-Requires: ((ess "15") (dash "1.8.0") (f "0.16.0"))
;; Keywords: extensions, ess

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'cl-lib)
(require 'ess-inf)
(require 'ess-custom)
(require 'dash)
(require 'f)


(defvar ess-r-spreadsheet-programs
  '("libreoffice" "gnumeric" "openoffice" "soffice")
  "List of spreadsheet programs in order of priority.")

(defvar ess-r-spreadsheet-temp-dir "/tmp/ess-r-spreadsheet/"
  "Temporary file directory.")

(defvar ess-r-spreadsheet--temp-file nil
  "Temporary file.")


(defun ess-r-spreadsheet--get-proc ()
  (if (stringp ess-local-process-name)
      (get-process ess-local-process-name)
    nil))

(defun ess-r-spreadsheet--get-obj-name ()
  (let ((obj))
    (setq obj (ess-read-object-name "R object:"))
    (substring-no-properties (car obj))))

(defun ess-r-spreadsheet--get-temp-file (obj-name)
  (let ((dir ess-r-spreadsheet-temp-dir)
        (temp-name))
    (f-mkdir dir)
    (setq temp-name (make-temp-name (concat obj-name "_")))
    ;; Return temp file path with ramdom suffix
    ;; e.g. /tmp/ess-r-spreadsheet/iris_URRqCX.csv
    (concat dir temp-name ".csv")))

(defun ess-r-spreadsheet--get-executable ()
  (->> ess-r-spreadsheet-programs
       (-map 'executable-find)
       (-non-nil)
       (car) ; return nil if no executable found
       ))

(cl-defun ess-r-spreadsheet--save ()
  (let ((proc (ess-r-spreadsheet--get-proc))
        (obj)
        (func)
        (cmd)
        (msg))

    ;; Check if the iESS process is running
    (unless (processp proc)
      (message "Process not started.")
      (return-from ess-r-spreadsheet--save))

    ;; Check library avilability
    (cond
     ((ess-boolean-command "'data.table' %in% installed.packages()\n")
      (setq func "data.table::fwrite"))
     ((ess-boolean-command "'readr' %in% installed.packages()\n")
      (setq func "readr::write_csv"))
     ((ess-boolean-command "'utils' %in% installed.packages()\n")
      (setq func "utils::write.csv"))
     (t
      (message "No save funcstions found.")
      (return-from ess-r-spreadsheet--save)))

    ;; Build R command
    ;; TODO command option
    (setq obj (ess-r-spreadsheet--get-obj-name))
    (setq ess-r-spreadsheet--temp-file (ess-r-spreadsheet--get-temp-file obj))
    (cond
     ((ess-boolean-command (format "is.data.frame(%s)\n" obj))
      (setq cmd (format "%s(%s, '%s')" func obj ess-r-spreadsheet--temp-file)))
     ((ess-boolean-command (format "is.matrix(%s) || is.atomic(%s)\n" obj obj))
      (setq cmd (format "%s(as.data.frame(%s), '%s')"
                        func obj ess-r-spreadsheet--temp-file)))
     ((ess-boolean-command (format "is.list(%s)\n" obj))
      (message "Use $ to select object inside the list.")
      (return-from ess-r-spreadsheet--save))
     (t
      (message "This data type is not supported.")
      (return-from ess-r-spreadsheet--save)))

    ;; Output message
    (setq msg (format "Saving as %s..." ess-r-spreadsheet--temp-file))
    (ess-send-string proc cmd nil msg)

    ;; Wait for the file readable
    ;; TODO wait timer for large obj
    (while (not (f-readable? ess-r-spreadsheet--temp-file))
      (sit-for 1))

    ;; Return t if sucess. Failed case return nil by (return-from)
    (message "Done.")
    t))

(cl-defun ess-r-spreadsheet--view ()
  (let ((exec (ess-r-spreadsheet--get-executable))
        (file ess-r-spreadsheet--temp-file))

    ;; Check if executable exists
    (unless exec
      (message "No spreadsheet executables found.")
      (return-from ess-r-spreadsheet--view))

    ;; Check if saved file exists
    (unless file
      (message "Could not find saved file.")
      (return-from ess-r-spreadsheet--view))

    (start-process "ess-r-spreadsheet" nil exec file)))

;;;###autoload
(defun ess-r-spreadsheet ()
  "View R's object in your spreadsheet program."
  (interactive)
  (when (ess-r-spreadsheet--save)
    (ess-r-spreadsheet--view)))

(provide 'ess-r-spreadsheet)

;;; ess-r-spreadsheet.el ends here
