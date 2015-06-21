# List Environment

A tabulated process environment editor

## Usage

Running:

<kbd>M-x list-environment</kbd>

will popup a buffer showing the current values in `process-environment` in a nice tabulated view.

## Editing Commands

While in `list-environment-mode` the following commands are available:

Keybindings  | Description
-------------|--------------------------
<kbd>s</kbd> | Call setenv interactively on the environment variable at point
<kbd>a</kbd> | Call setenv without a default name
<kbd>d</kbd> | Call setenv with prefix to clear variable at point

It also supports all the normal `tabulated-list-mode` commands, use <kbd>h</kbd> to run `describe-mode` to view bindings.

# License

Copyright © 2015 Charles L.G. Comstock

Distributed under the GNU General Public License, version 3
