#!/usr/bin/emacs --script
(message "The current directory is %s" default-directory)
(macroexpand 'cl-loop)
