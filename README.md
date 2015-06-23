# List Environment

[![MELPA](http://melpa.org/packages/list-environment-badge.svg)](http://melpa.org/#/list-environment)

A tabulated process environment editor

## Usage

Running:

<kbd>M-x list-environment</kbd>

will popup a buffer showing the current values in `process-environment` in a nice tabulated view.

## Install

After adding the [MELPA](melpa.org) package archive:

<kbd>M-x package-install [RET] list-environment [RET]

## Editing Commands

While in `list-environment-mode` the following commands are available:

Keybindings  | Description
-------------|--------------------------
<kbd>s</kbd> | Set the environment variable at point to a new value
<kbd>a</kbd> | Add a new environment variable
<kbd>d</kbd> | Clear environment variable at point

It also supports all the normal `tabulated-list-mode` commands, use <kbd>h</kbd> to run `describe-mode` to view bindings.

# License

Copyright © 2015 Charles L.G. Comstock

Distributed under the GNU General Public License, version 3
