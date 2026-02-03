;;; invoice-ninga-api.el --- Invoice Ninja API client -*- lexical-binding: t; -*-

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

;; Core API client for Invoice Ninja.  Handles HTTP communication,
;; authentication, and response parsing.

;;; Code:

(require 'json)
(require 'url)

;;; Customization

(defgroup invoice-ninga-api nil
  "Invoice Ninja API configuration."
  :group 'invoice-ninga
  :prefix "invoice-ninga-api-")

(defcustom invoice-ninga-api-url nil
  "Base URL for Invoice Ninja API (e.g., \"http://localhost:8012\")."
  :type '(choice (const nil) string)
  :group 'invoice-ninga-api)

(defcustom invoice-ninga-api-token nil
  "API token for Invoice Ninja authentication."
  :type '(choice (const nil) string)
  :group 'invoice-ninga-api)

;;; Internal Functions

(defun invoice-ninga-api--parse-response (buffer)
  "Parse HTTP response from BUFFER.
Returns a cons cell (STATUS-CODE . PARSED-JSON)."
  (with-current-buffer buffer
    (goto-char (point-min))
    (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (error "Invalid HTTP response"))
    (let ((status-code (string-to-number (match-string 1))))
      (unless (re-search-forward "\n\n" nil t)
        (error "Could not find response body"))
      (let* ((body (buffer-substring-no-properties (point) (point-max)))
             (json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (response (condition-case nil
                           (json-read-from-string body)
                         (error nil))))
        (cons status-code response)))))

;;; Public Functions

(defun invoice-ninga-api-request (method endpoint &optional data)
  "Make HTTP request to Invoice Ninja API.
METHOD is the HTTP method (GET, POST, PUT, DELETE).
ENDPOINT is the API endpoint (e.g., \"/api/v1/tasks\").
DATA is an optional alist to send as JSON body.
Returns parsed JSON response or signals an error."
  (unless invoice-ninga-api-url
    (user-error "Variable `invoice-ninga-api-url' is not configured"))
  (unless invoice-ninga-api-token
    (user-error "Variable `invoice-ninga-api-token' is not configured"))
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("X-API-TOKEN" . ,invoice-ninga-api-token)
            ("X-Requested-With" . "XMLHttpRequest")
            ("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data
          (when data (encode-coding-string (json-encode data) 'utf-8)))
         (url (concat (string-trim-right invoice-ninga-api-url "/") endpoint))
         (buffer (url-retrieve-synchronously url t)))
    (unless buffer
      (error "Failed to connect to Invoice Ninja API"))
    (unwind-protect
        (let* ((parsed (invoice-ninga-api--parse-response buffer))
               (status-code (car parsed))
               (response (cdr parsed)))
          (if (and (>= status-code 200) (< status-code 300))
              response
            (error "API error %d: %s"
                   status-code
                   (or (alist-get 'message response)
                       (json-encode response)))))
      (kill-buffer buffer))))

(provide 'invoice-ninga-api)
;;; invoice-ninga-api.el ends here
