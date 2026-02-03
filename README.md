# invoice-ninga.el

Export org-mode tasks directly to Invoice Ninja. 

## What it does

Parses org-mode task headings and their clock entries, then pushes them to Invoice Ninja via API or exports to CSV for manual import.

## Installation

Clone the repo and add to your load path:

```elisp
(add-to-list 'load-path "/path/to/invoice-ninga.el")
(require 'invoice-ninga)
```

## Configuration

```elisp
(setq invoice-ninga-api-url "http://localhost:8012")
(setq invoice-ninga-api-token "your-api-token")
```

## Usage

Structure your org tasks like this:

```org
* TODO Website Redesign
  :PROPERTIES:
  :NUMBER: 001
  :CLIENT: Acme Corp
  :STATE: billable
  :END:
  :LOGBOOK:
  CLOCK: [2024-01-15 Mon 09:00]--[2024-01-15 Mon 11:30] =>  2:30
  :END:
  Task description goes here.
```

Select a region containing task headlines, then:

- `M-x invoice-ninga-export-tasks-api` — push directly to Invoice Ninja
- `M-x invoice-ninga-export-tasks` — export to CSV

## Commands

| Command | Description |
|---------|-------------|
| `invoice-ninga-export-tasks-api` | Export selected tasks via API |
| `invoice-ninga-export-tasks` | Export selected tasks to CSV |
| `invoice-ninga-clear-cache` | Clear cached entity data |

## Task Properties

| Property | Description |
|----------|-------------|
| `:NUMBER:` | Task reference number |
| `:CLIENT:` | Client name (matched against Invoice Ninja clients) |
| `:STATE:` | Billing state (billable, non-billable, completed) |

Clock entries in the LOGBOOK drawer are parsed and converted to Invoice Ninja time logs automatically.

## Requirements

- Emacs 27.1+
- Invoice Ninja instance with API access
