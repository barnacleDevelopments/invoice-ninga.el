# invoice-ninga.el
Emacs package for interacting with invoice-ninga app.

# Task Export 
Task export function for exporting tasks in org-mode to a excel spreadsheet so they can be imported into Invoice Ninga. 

## Export fields
- status
- number
- client
- description
- duration
- state
 
## How to use 
Highlights the org-headers you wish to export. Call the `invoice-ninga-export-tasks` function. An excel spreadsheet is created in the directory of your org file or at the specified directory by setting the `invoice-ninga-export-task-dir`.
