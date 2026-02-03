;;; invoice-ninga-org.el --- Org-mode parsing utilities -*- lexical-binding: t; -*-

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

;; Reusable org-mode parsing utilities for Invoice Ninja integration.
;; Provides functions to extract timestamps, clock entries, body content,
;; and properties from org headlines.

;;; Code:

(require 'org)
(require 'org-element)

;;; Timestamp Parsing

(defun invoice-ninga-org-parse-timestamp (timestamp-str)
  "Parse org TIMESTAMP-STR to Unix timestamp.
Handles format like [2024-01-15 Mon 09:00].
Returns Unix timestamp as float or nil if parsing fails."
  (when (string-match
         "\\[\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) [A-Za-z]+ \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\]"
         timestamp-str)
    (let ((year (string-to-number (match-string 1 timestamp-str)))
          (month (string-to-number (match-string 2 timestamp-str)))
          (day (string-to-number (match-string 3 timestamp-str)))
          (hour (string-to-number (match-string 4 timestamp-str)))
          (minute (string-to-number (match-string 5 timestamp-str))))
      (float-time (encode-time 0 minute hour day month year)))))

;;; Clock Entry Parsing

(defun invoice-ninga-org-parse-clock-entries (headline)
  "Parse CLOCK entries from HEADLINE element.
Returns a list of plists with :start and :end Unix timestamps."
  (let ((entries nil)
        (contents-begin (org-element-property :contents-begin headline))
        (contents-end (org-element-property :contents-end headline)))
    (when (and contents-begin contents-end)
      (save-excursion
        (goto-char contents-begin)
        (while (re-search-forward
                "CLOCK: \\(\\[.*?\\]\\)--\\(\\[.*?\\]\\) =>[ \t]*\\([0-9]+\\):\\([0-9]+\\)"
                contents-end t)
          (let* ((start-str (match-string 1))
                 (end-str (match-string 2))
                 (start-ts (invoice-ninga-org-parse-timestamp start-str))
                 (end-ts (invoice-ninga-org-parse-timestamp end-str)))
            (when (and start-ts end-ts)
              (push (list :start start-ts :end end-ts) entries))))))
    (nreverse entries)))

;;; Body Content Extraction

(defun invoice-ninga-org-get-body-content (headline)
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

;;; Property Extraction

(defun invoice-ninga-org-get-property (headline property)
  "Get PROPERTY value from HEADLINE element.
PROPERTY should be a keyword symbol like :CLIENT or :NUMBER."
  (org-element-property property headline))

(defun invoice-ninga-org-get-properties (headline properties)
  "Get multiple PROPERTIES from HEADLINE element.
PROPERTIES is a list of keyword symbols.
Returns an alist of (property . value) pairs."
  (mapcar (lambda (prop)
            (cons prop (org-element-property prop headline)))
          properties))

;;; Region Parsing

(defun invoice-ninga-org-parse-region (region-start region-end extractor)
  "Parse org headlines between REGION-START and REGION-END.
EXTRACTOR is a function called with each headline element.
Returns a list of results from EXTRACTOR for each headline."
  (let ((entries nil)
        (tree (org-element-parse-buffer)))
    (org-element-map tree 'headline
      (lambda (headline)
        (let ((begin (org-element-property :begin headline)))
          ;; Include any headline that starts within the region
          (when (and (>= begin region-start)
                     (< begin region-end))
            (push (funcall extractor headline) entries)))))
    (nreverse entries)))

(defun invoice-ninga-org-get-todo-keyword (headline)
  "Get the TODO keyword from HEADLINE element."
  (org-element-property :todo-keyword headline))

(defun invoice-ninga-org-get-title (headline)
  "Get the title from HEADLINE element."
  (org-element-property :raw-value headline))

(provide 'invoice-ninga-org)
;;; invoice-ninga-org.el ends here
