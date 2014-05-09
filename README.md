dcd-mode
========

An emacs mode for use with the DCD tool

Currently supports autocomletion and goto-symbol.  Documentation of symbol at point is still in progress.

To configure, add the following to your .emacs

(require 'dcd-mode)
(add-hook 'd-mode-hook
          (lambda ()
            (dcd-mode 1)))


