;;; invoice-ninga.el --- Export org-mode tasks to Invoice Ninja -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Devin Davis
;; Version: 0.2.0
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

;; This package exports org-mode tasks to Invoice Ninja.  It extracts
;; task properties and calculates duration from org clock entries in
;; the LOGBOOK drawer.
;;
;; Two export methods are available:
;;   1. API export (recommended): Direct export via Invoice Ninja REST API
;;   2. CSV export (fallback): Export to CSV file for manual import
;;
;; API Usage:
;;   1. Configure API URL and token:
;;      (setq invoice-ninga-api-url "http://localhost:8012")
;;      (setq invoice-ninga-api-token "your-api-token")
;;   2. Select a region containing org-mode task headers
;;   3. Run M-x invoice-ninga-export-tasks-api
;;
;; CSV Usage:
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
;;
;; Module Structure:
;;   - invoice-ninga-api.el: Core API client (HTTP requests, auth)
;;   - invoice-ninga-entities.el: Entity framework (generic CRUD, caching)
;;   - invoice-ninga-org.el: Org-mode parsing utilities
;;   - invoice-ninga-tasks.el: Task-specific logic and export commands

;;; Code:

(require 'invoice-ninga-api)
(require 'invoice-ninga-entities)
(require 'invoice-ninga-org)
(require 'invoice-ninga-tasks)

;;; Customization Group

(defgroup invoice-ninga nil
  "Export `org-mode' tasks to Invoice Ninja."
  :group 'org
  :prefix "invoice-ninga-")

;;; Public Commands

;;;###autoload
(defun invoice-ninga-clear-cache ()
  "Clear all cached Invoice Ninja entity data.
Use this if data has changed in Invoice Ninja."
  (interactive)
  (invoice-ninga-entity-clear-cache)
  (message "Invoice Ninja cache cleared"))

(provide 'invoice-ninga)
;;; invoice-ninga.el ends here
