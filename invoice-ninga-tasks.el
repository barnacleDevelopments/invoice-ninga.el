;;; invoice-ninga-tasks.el --- Task export to Invoice Ninja -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Devin Davis

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Task-specific logic for exporting org-mode tasks to Invoice Ninja.
;; Provides both API and CSV export functionality.

;;; Code:

(require 'invoice-ninga-api)
(require 'invoice-ninga-entities)
(require 'invoice-ninga-org)

;;; Customization

(defgroup invoice-ninga-tasks nil
  "Task export settings for Invoice Ninja."
  :group 'invoice-ninga
  :prefix "invoice-ninga-task-")

(defcustom invoice-ninga-export-task-dir nil
  "Directory for exported CSV files.
If nil, defaults to the directory of the current org file."
  :type '(choice (const :tag "Same as org file" nil)
                 (directory :tag "Custom directory"))
  :group 'invoice-ninga-tasks)

(defcustom invoice-ninga-status-mapping
  '(("TODO" . "Ready to do")
    ("IN-PROGRESS" . "In Progress")
    ("DONE" . "Done"))
  "Alist mapping org TODO keywords to Invoice Ninja status values.
Each element is (ORG-KEYWORD . INVOICE-NINJA-STATUS).
Keywords not in this list will be passed through unchanged."
  :type '(alist :key-type string :value-type string)
  :group 'invoice-ninga-tasks)

;;; Status Mapping

(defun invoice-ninga-task--map-status (org-keyword)
  "Map ORG-KEYWORD to Invoice Ninja status.
Uses `invoice-ninga-status-mapping' to convert org TODO keywords.
Returns the mapped status or the original keyword if no mapping exists."
  (or (cdr (assoc org-keyword invoice-ninga-status-mapping))
      (or org-keyword "")))

;;; Time Log Building

(defun invoice-ninga-task-build-time-log (headline)
  "Build Invoice Ninja time_log JSON string for HEADLINE.
Parses CLOCK entries and returns a JSON string.
Format: [[start_ts, end_ts, \"description\", is_billable], ...].
Invoice Ninja stores time_log as a JSON string in the database."
  (let ((clock-entries (invoice-ninga-org-parse-clock-entries headline)))
    (if clock-entries
        (concat "["
                (string-join
                 (mapcar (lambda (entry)
                           (format "[%d,%d,\"\",true]"
                                   (truncate (plist-get entry :start))
                                   (truncate (plist-get entry :end))))
                         clock-entries)
                 ",")
                "]")
      "[]")))

;;; Headline to Task Conversion

(defun invoice-ninga-task-from-org-headline (headline)
  "Convert org HEADLINE to Invoice Ninja task properties.
Returns an alist with keys: status, number, client, description,
time_log, state."
  (let* ((todo-keyword (invoice-ninga-org-get-todo-keyword headline))
         (number (invoice-ninga-org-get-property headline :NUMBER))
         (client (invoice-ninga-org-get-property headline :CLIENT))
         (state (invoice-ninga-org-get-property headline :STATE))
         (description (invoice-ninga-org-get-body-content headline))
         (time-log (invoice-ninga-task-build-time-log headline)))
    `((status . ,(invoice-ninga-task--map-status todo-keyword))
      (number . ,(or number ""))
      (client . ,(or client ""))
      (description . ,(or description ""))
      (time_log . ,time-log)
      (state . ,(or state "")))))

;;; API Task Creation

(defun invoice-ninga-task-create (entry)
  "Create a task in Invoice Ninja from ENTRY alist.
ENTRY should contain keys: description, time_log, client, status.
Returns the created task data or signals an error."
  (let* ((description (alist-get 'description entry))
         (time-log (alist-get 'time_log entry))
         (client-name (alist-get 'client entry))
         (status-name (alist-get 'status entry))
         (client-id (invoice-ninga-entity-find-by-name 'clients client-name))
         (status-id (invoice-ninga-entity-find-by-name 'task_statuses status-name))
         (payload `((description . ,description))))
    ;; Only include time_log if it has entries
    (when (and time-log (not (string= time-log "[]")))
      (push (cons 'time_log time-log) payload))
    (when client-id
      (push (cons 'client_id client-id) payload))
    (when status-id
      (push (cons 'status_id status-id) payload))
    (invoice-ninga-entity-create 'tasks payload)))

;;; CSV Export

(defun invoice-ninga-task--escape-csv-field (field)
  "Escape FIELD for CSV output.
Wraps in quotes if contains comma, quote, or newline.
Doubles any existing quotes."
  (let ((str (if (numberp field)
                 (number-to-string field)
               (or field ""))))
    (if (string-match-p "[,\"\n\r]" str)
        (concat "\"" (replace-regexp-in-string "\"" "\"\"" str) "\"")
      str)))

(defun invoice-ninga-task--write-csv (entries output-path)
  "Write ENTRIES to CSV file at OUTPUT-PATH.
ENTRIES is a list of alists with keys: status, number, client,
description, time_log, state."
  (with-temp-file output-path
    ;; Write header row
    (insert "status,number,client,description,time_log,state\n")
    ;; Write data rows
    (dolist (entry entries)
      (insert (mapconcat
               #'invoice-ninga-task--escape-csv-field
               (list (alist-get 'status entry)
                     (alist-get 'number entry)
                     (alist-get 'client entry)
                     (alist-get 'description entry)
                     (alist-get 'time_log entry)
                     (alist-get 'state entry))
               ",")
              "\n"))))

;;; Region Selection Helper

(defun invoice-ninga-task--get-export-region ()
  "Get the region to export tasks from.
Returns a cons cell (START . END) or signals an error."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (if (yes-or-no-p "No region selected. Export current subtree? ")
        (save-excursion
          (org-back-to-heading t)
          (let ((start (point)))
            (org-end-of-subtree t t)
            (cons start (point))))
      (user-error "No region selected"))))

;;; Public Export Commands

;;;###autoload
(defun invoice-ninga-export-tasks ()
  "Export `org-mode' tasks in region to CSV for Invoice Ninja.
If region is active, exports all headlines within the region.
Otherwise, prompts to export the current subtree."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in an org-mode buffer"))
  (let* ((region (invoice-ninga-task--get-export-region))
         (region-start (car region))
         (region-end (cdr region))
         (entries (invoice-ninga-org-parse-region
                   region-start region-end
                   #'invoice-ninga-task-from-org-headline))
         (export-dir (or invoice-ninga-export-task-dir
                         (file-name-directory (buffer-file-name))))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (output-path (expand-file-name
                       (format "invoice-ninga-export-%s.csv" timestamp)
                       export-dir)))
    (if (null entries)
        (user-error "No task entries found in selection")
      ;; Ensure export directory exists
      (unless (file-directory-p export-dir)
        (make-directory export-dir t))
      ;; Write CSV
      (invoice-ninga-task--write-csv entries output-path)
      (message "Exported %d tasks to %s" (length entries) output-path))))

;;;###autoload
(defun invoice-ninga-export-tasks-api ()
  "Export `org-mode' tasks in region to Invoice Ninja via API.
If region is active, exports all headlines within the region.
Otherwise, prompts to export the current subtree.

Requires `invoice-ninga-api-url' and `invoice-ninga-api-token' to be set."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in an org-mode buffer"))
  (unless invoice-ninga-api-url
    (user-error "Please set `invoice-ninga-api-url' before exporting"))
  (unless invoice-ninga-api-token
    (user-error "Please set `invoice-ninga-api-token' before exporting"))
  (let* ((region (invoice-ninga-task--get-export-region))
         (region-start (car region))
         (region-end (cdr region))
         (entries (invoice-ninga-org-parse-region
                   region-start region-end
                   #'invoice-ninga-task-from-org-headline))
         (total (length entries))
         (success-count 0)
         (failed nil))
    (if (null entries)
        (user-error "No task entries found in selection")
      ;; Create each task via API
      (dolist (entry entries)
        (condition-case err
            (progn
              (invoice-ninga-task-create entry)
              (setq success-count (1+ success-count)))
          (error
           (push (cons (alist-get 'description entry)
                       (error-message-string err))
                 failed))))
      ;; Report results
      (if failed
          (progn
            (message "Exported %d/%d tasks. %d failed."
                     success-count total (length failed))
            (with-output-to-temp-buffer "*Invoice Ninga Export Errors*"
              (princ "Failed tasks:\n\n")
              (dolist (f (nreverse failed))
                (princ (format "- %s\n  Error: %s\n\n"
                               (or (car f) "(no description)")
                               (cdr f))))))
        (message "Successfully exported %d tasks to Invoice Ninja"
                 success-count)))))

(provide 'invoice-ninga-tasks)
;;; invoice-ninga-tasks.el ends here
