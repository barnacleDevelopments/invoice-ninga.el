;;; invoice-ninga-entities.el --- Invoice Ninja entity framework -*- lexical-binding: t; -*-

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

;; Generic CRUD operations for Invoice Ninja entities.  Provides a
;; unified framework for working with clients, tasks, projects, and
;; other entity types.

;;; Code:

(require 'invoice-ninga-api)

;;; Entity Registry

(defvar invoice-ninga-entities
  '((clients . ((endpoint . "/api/v1/clients")
                (name-field . name)
                (id-field . id)))
    (tasks . ((endpoint . "/api/v1/tasks")
              (name-field . description)
              (id-field . id)))
    (task_statuses . ((endpoint . "/api/v1/task_statuses")
                      (name-field . name)
                      (id-field . id)))
    (projects . ((endpoint . "/api/v1/projects")
                 (name-field . name)
                 (id-field . id))))
  "Registry of Invoice Ninja entity types.
Each entry is (TYPE . PROPERTIES) where PROPERTIES is an alist with:
  - endpoint: API endpoint path
  - name-field: Symbol for the field containing the entity name
  - id-field: Symbol for the field containing the entity ID")

;;; Cache

(defvar invoice-ninga--entity-cache (make-hash-table :test 'eq)
  "Hash table caching entity data by type.
Keys are entity type symbols, values are alists of (name . id).")

(defun invoice-ninga-entity--get-config (entity-type)
  "Get configuration for ENTITY-TYPE from registry.
Signals an error if entity type is not registered."
  (let ((config (alist-get entity-type invoice-ninga-entities)))
    (unless config
      (error "Unknown entity type: %s" entity-type))
    config))

;;; CRUD Operations

(defun invoice-ninga-entity-list (entity-type &optional force-refresh)
  "Get all entities of ENTITY-TYPE.
Returns cached data if available, unless FORCE-REFRESH is non-nil.
Returns the raw list of entity alists from the API."
  (let ((cached (gethash entity-type invoice-ninga--entity-cache)))
    (if (and cached (not force-refresh))
        cached
      (let* ((config (invoice-ninga-entity--get-config entity-type))
             (endpoint (alist-get 'endpoint config))
             (response (invoice-ninga-api-request "GET" endpoint))
             (data (alist-get 'data response)))
        (puthash entity-type data invoice-ninga--entity-cache)
        data))))

(defun invoice-ninga-entity-get (entity-type id)
  "Get a single entity of ENTITY-TYPE by ID.
Returns the entity alist or nil if not found."
  (let* ((config (invoice-ninga-entity--get-config entity-type))
         (endpoint (concat (alist-get 'endpoint config) "/" id))
         (response (invoice-ninga-api-request "GET" endpoint)))
    (alist-get 'data response)))

(defun invoice-ninga-entity-create (entity-type data)
  "Create a new entity of ENTITY-TYPE with DATA.
DATA is an alist of field-value pairs.
Returns the created entity alist."
  (let* ((config (invoice-ninga-entity--get-config entity-type))
         (endpoint (alist-get 'endpoint config))
         (response (invoice-ninga-api-request "POST" endpoint data)))
    ;; Invalidate cache for this entity type
    (remhash entity-type invoice-ninga--entity-cache)
    (alist-get 'data response)))

(defun invoice-ninga-entity-update (entity-type id data)
  "Update entity of ENTITY-TYPE with ID using DATA.
DATA is an alist of field-value pairs to update.
Returns the updated entity alist."
  (let* ((config (invoice-ninga-entity--get-config entity-type))
         (endpoint (concat (alist-get 'endpoint config) "/" id))
         (response (invoice-ninga-api-request "PUT" endpoint data)))
    ;; Invalidate cache for this entity type
    (remhash entity-type invoice-ninga--entity-cache)
    (alist-get 'data response)))

(defun invoice-ninga-entity-delete (entity-type id)
  "Delete entity of ENTITY-TYPE with ID.
Returns the API response."
  (let* ((config (invoice-ninga-entity--get-config entity-type))
         (endpoint (concat (alist-get 'endpoint config) "/" id))
         (response (invoice-ninga-api-request "DELETE" endpoint)))
    ;; Invalidate cache for this entity type
    (remhash entity-type invoice-ninga--entity-cache)
    response))

;;; Lookup Helpers

(defun invoice-ninga-entity-find-by-name (entity-type name)
  "Find entity ID by NAME in ENTITY-TYPE.
Returns the ID string or nil if not found."
  (when (and name (not (string-empty-p name)))
    (let* ((config (invoice-ninga-entity--get-config entity-type))
           (name-field (alist-get 'name-field config))
           (id-field (alist-get 'id-field config))
           (entities (invoice-ninga-entity-list entity-type)))
      (cl-loop for entity in entities
               when (equal (alist-get name-field entity) name)
               return (alist-get id-field entity)))))

(defun invoice-ninga-entity-find-by-id (entity-type id)
  "Find entity by ID in ENTITY-TYPE.
Returns the entity alist or nil if not found.
Uses cached data if available."
  (when id
    (let* ((config (invoice-ninga-entity--get-config entity-type))
           (id-field (alist-get 'id-field config))
           (entities (invoice-ninga-entity-list entity-type)))
      (cl-loop for entity in entities
               when (equal (alist-get id-field entity) id)
               return entity))))

(defun invoice-ninga-entity-names (entity-type)
  "Get list of all entity names for ENTITY-TYPE.
Returns a list of name strings."
  (let* ((config (invoice-ninga-entity--get-config entity-type))
         (name-field (alist-get 'name-field config))
         (entities (invoice-ninga-entity-list entity-type)))
    (mapcar (lambda (entity) (alist-get name-field entity)) entities)))

;;; Cache Management

(defun invoice-ninga-entity-clear-cache (&optional entity-type)
  "Clear entity cache.
If ENTITY-TYPE is provided, clear only that type's cache.
Otherwise, clear all cached entities."
  (if entity-type
      (remhash entity-type invoice-ninga--entity-cache)
    (clrhash invoice-ninga--entity-cache)))

(provide 'invoice-ninga-entities)
;;; invoice-ninga-entities.el ends here
