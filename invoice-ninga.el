;;; invoice-ninga.el --- Export org-mode tasks to CSV for Invoice Ninja -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Devin Davis
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines, calendar, wp
;; URL: https://github.com/devindavis/invoice-ninga.el

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

;; This package exports org-mode tasks to CSV format suitable for
;; importing into Invoice Ninja.  It extracts task properties and
;; calculates duration from org clock entries in the LOGBOOK drawer.
;;
;; Usage:
;;   1. Select a region containing org-mode task headers
;;   2. Run M-x invoice-ninga-export-tasks
;;   3. The CSV file will be written to the configured directory
;;
;; Expected org format:
;;   * TODO Task Title
;;     :PROPERTIES:
;;     :NUMBER: 001
;;     :CLIENT: Acme Corp
;;     :STATE: billable
;;     :END:
;;     :LOGBOOK:
;;     CLOCK: [2024-01-15 Mon 09:00]--[2024-01-15 Mon 11:30] =>  2:30
;;     :END:
;;     This body text becomes the description field.

;;; Code:

(require 'org)
(require 'org-element)

;;; Customization

(defgroup invoice-ninga nil
  "Export `org-mode' tasks to CSV for Invoice Ninja."
  :group 'org
  :prefix "invoice-ninga-")

(defcustom invoice-ninga-export-task-dir nil
  "Directory for exported CSV files.
If nil, defaults to the directory of the current org file."
  :type '(choice (const :tag "Same as org file" nil)
                 (directory :tag "Custom directory"))
  :group 'invoice-ninga)

(defcustom invoice-ninga-status-mapping
  '(("TODO" . "Ready to do")
    ("IN-PROGRESS" . "In Progress")
    ("DONE" . "Done"))
  "Alist mapping org TODO keywords to Invoice Ninja status values.
Each element is (ORG-KEYWORD . INVOICE-NINJA-STATUS).
Keywords not in this list will be passed through unchanged."
  :type '(alist :key-type string :value-type string)
  :group 'invoice-ninga)

;;; Internal Functions

(defun invoice-ninga--calculate-duration (headline)
  "Calculate total clocked duration for HEADLINE in minutes.
Parses CLOCK entries from the LOGBOOK drawer and sums their durations."
  (let ((total-minutes 0)
        (contents-begin (org-element-property :contents-begin headline))
        (contents-end (org-element-property :contents-end headline)))
    (when (and contents-begin contents-end)
      (save-excursion
        (goto-char contents-begin)
        (while (re-search-forward
                "CLOCK: \\[.*?\\]--\\[.*?\\] =>[ \t]*\\([0-9]+\\):\\([0-9]+\\)"
                contents-end t)
          (let ((hours (string-to-number (match-string 1)))
                (minutes (string-to-number (match-string 2))))
            (setq total-minutes (+ total-minutes (* hours 60) minutes))))))
    total-minutes))

(defun invoice-ninga--get-body-content (headline)
  "Extract body content from HEADLINE, excluding drawers.
Returns the text content below the headline that is not in a drawer."
  (let ((contents-begin (org-element-property :contents-begin headline))
        (contents-end (org-element-property :contents-end headline))
        (body-parts nil))
    (when (and contents-begin contents-end)
      (org-element-map (org-element-contents headline) '(paragraph plain-list)
        (lambda (el)
          (let ((parent (org-element-property :parent el)))
            ;; Only include if direct child of headline (not in a drawer)
            (when (eq (org-element-type parent) 'section)
              (let ((begin (org-element-property :begin el))
                    (end (org-element-property :end el)))
                (when (and begin end)
                  (push (string-trim
                         (buffer-substring-no-properties begin end))
                        body-parts))))))
        nil nil 'headline))
    (string-join (nreverse body-parts) "\n")))

(defun invoice-ninga--get-entry-properties (headline)
  "Extract properties from HEADLINE element.
Returns an alist with keys: status, number, client, description, duration, state."
  (let* ((todo-keyword (org-element-property :todo-keyword headline))
         (number (org-element-property :NUMBER headline))
         (client (org-element-property :CLIENT headline))
         (state (org-element-property :STATE headline))
         (description (invoice-ninga--get-body-content headline))
         (duration (invoice-ninga--calculate-duration headline)))
    `((status . ,(invoice-ninga--map-status todo-keyword))
      (number . ,(or number ""))
      (client . ,(or client ""))
      (description . ,(or description ""))
      (duration . ,duration)
      (state . ,(or state "")))))

(defun invoice-ninga--parse-entries (region-start region-end)
  "Parse org entries between REGION-START and REGION-END.
Returns a list of alists, each containing properties for one entry."
  (let ((entries nil)
        (tree (org-element-parse-buffer)))
    (org-element-map tree 'headline
      (lambda (headline)
        (let ((begin (org-element-property :begin headline))
              (end (org-element-property :end headline)))
          ;; Only process headlines that overlap with the region
          (when (and (<= begin region-end)
                     (>= end region-start)
                     ;; Only top-level headlines within the selection
                     (<= begin region-end)
                     (>= begin region-start))
            (push (invoice-ninga--get-entry-properties headline) entries))))
      nil nil 'headline)
    (nreverse entries)))

(defun invoice-ninga--escape-csv-field (field)
  "Escape FIELD for CSV output.
Wraps in quotes if contains comma, quote, or newline.
Doubles any existing quotes."
  (let ((str (if (numberp field)
                 (number-to-string field)
               (or field ""))))
    (if (string-match-p "[,\"\n\r]" str)
        (concat "\"" (replace-regexp-in-string "\"" "\"\"" str) "\"")
      str)))

(defun invoice-ninga--write-csv (entries output-path)
  "Write ENTRIES to CSV file at OUTPUT-PATH.
ENTRIES is a list of alists with keys: status, number, client, description, duration, state."
  (with-temp-file output-path
    ;; Write header row
    (insert "status,number,client,description,duration,state\n")
    ;; Write data rows
    (dolist (entry entries)
      (insert (mapconcat
               #'invoice-ninga--escape-csv-field
               (list (alist-get 'status entry)
                     (alist-get 'number entry)
                     (alist-get 'client entry)
                     (alist-get 'description entry)
                     (alist-get 'duration entry)
                     (alist-get 'state entry))
               ",")
              "\n"))))

;;; Public Functions

;;;###autoload
(defun invoice-ninga--map-status (org-keyword)
  "Map ORG-KEYWORD to Invoice Ninja status using `invoice-ninga-status-mapping'.
Returns the mapped status or the original keyword if no mapping exists."
  (or (cdr (assoc org-keyword invoice-ninga-status-mapping))
      (or org-keyword "")))

;;;###autoload
(defun invoice-ninga-export-tasks ()
  "Export `org-mode' tasks in region to CSV for Invoice Ninja.
If region is active, exports all headlines within the region.
Otherwise, prompts to export the current subtree."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in an org-mode buffer"))
  (let (region-start region-end)
    ;; Determine region to export
    (if (use-region-p)
        (setq region-start (region-beginning)
              region-end (region-end))
      (if (yes-or-no-p "No region selected. Export current subtree? ")
          (save-excursion
            (org-back-to-heading t)
            (setq region-start (point))
            (org-end-of-subtree t t)
            (setq region-end (point)))
        (user-error "No region selected")))
    ;; Parse entries
    (let* ((entries (invoice-ninga--parse-entries region-start region-end))
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
        (invoice-ninga--write-csv entries output-path)
        (message "Exported %d tasks to %s" (length entries) output-path)))))

(provide 'invoice-ninga)
;;; invoice-ninga.el ends here
