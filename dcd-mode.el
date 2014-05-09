;;; dcd-mode.el --- Interact with the DCD command-line tool.

;; Copyright © 2014 John Maschmeyer

;; Author: John Maschmeyer
;; URL: https://github.com/jmaschme/dcd-mode
;; Keywords: dlang
;; Version: 0.1.0
;; Package-Requires: ()

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides dcd-mode.  It provides access to the DCD tool, including
;; auto-completion, symbol declaration lookup, and DDoc based documentation viewing.
;;
;;; Code:

(require 'cl)
(require 'yasnippet)

(defvar dcd-server-process nil
  "The server process.")

(defvar dcd-client "dcd-client"
  "The name of the client application.")

(defvar dcd-buffer nil
  "The current user buffer.")

(defvar dcd-point nil
  "The current point in the user buffer.")

(defvar dcd-current-fun-match nil)
(defvar dcd-fun-snippets nil)
(defvar dcd-fun-help nil)

(defvar dcd-init-complete nil)

(defun dcd-init ()
  "Initialize dcd-mode."
  (unless dcd-init-complete
    (unless dcd-server-process
      (dcd-init-server))
    (unless yas-minor-mode
      (yas-reload-all)
      (yas-minor-mode))
    (setq dcd-init-complete t)
    (auto-complete-mode 1)
    (setq ac-sources (append '(ac-source-dcd) ac-sources))
    (dcd-add-import-path (file-name-directory (buffer-file-name)))))

(defsubst ac-in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))

(defun dcd-get-function-completion (fun)
  "Gets the full name and signature of the function being completed."
  (setq dcd-current-fun-match fun)
  (with-temp-buffer
    (insert-buffer dcd-buffer)
    (goto-char dcd-point)
    (insert (concat fun "("))
    (apply 'dcd-call-with-buffer 
	   "*dcd-temp*"
	   (append '("-c") 
		   (list (format "%s" (point)))))
    (dcd-parse-output "*dcd-temp*")))

(defun dcd-snippet-from-fun (fun)
  "Given the function FUN, return a yasnippet formatted snippet that expands into FUN."
  (let ((s (dcd-remove-return-type fun)))
    (setq s (replace-regexp-in-string "(\\(.+\\))" "(${\\1})" s))
    (setq s (replace-regexp-in-string ", \\.\\.\\." "}, ${..." s))))

(defun dcd-get-completion-candidate (val type)
  "Gets the full name for a given completion candidate."
  (if (string-equal type "f")
      (let ((full-fun (dcd-get-function-completion val)))
	(setq dcd-fun-snippets
	      (append dcd-fun-snippets
		      (pairlis (mapcar 'dcd-remove-return-type full-fun) (mapcar 'dcd-snippet-from-fun full-fun))))
	(identity (mapcar 'dcd-remove-return-type full-fun)))
    (list val)))

(defun dcd-parse-completion-list (data)
  "Takes a list of completion elements and returns the values that should be displayed."
  (when data
    (let ((val (car (split-string (car data))))
	  (type (car (cdr (split-string (car data))))))
      (append (dcd-get-completion-candidate val type)
	     (dcd-parse-completion-list (cdr data))))))

(defun dcd-remove-return-type (fun)
  "Reorders a function to put the return type at the end."
  (let ((x (string-match (concat "\\([[:blank:]]+\\)" dcd-current-fun-match "(.*)$" ) fun)))
    (when x
      (replace-regexp-in-string "\\`[ \t\n]*" 
				"" 
				(replace-regexp-in-string "[ \t\n]*\\'" 
							  "" (substring fun x))))))

(defun dcd-parse-paren-list (data)
  "Takes a list of calltips and returns the values that should be displayed"
  (remove-if (lambda (s) (string= "" s)) data))

(defun dcd-parse-output (buf)
  "Parses the output of dcd-client."
  (with-current-buffer buf
    (let ((out (split-string (buffer-string) "[\n\r]+")))
      (when out
      	(cond ((string-equal (car out) "identifiers")
      	       (dcd-parse-completion-list (cdr out)))
      	      ((string-equal (car out) "calltips")
      	       (dcd-parse-paren-list (cdr out)))
      	      (t nil))))))

(defun dcd-init-server ()
  "Initialize the DCD server."
  (unless dcd-server-process
    (setq dcd-server-process 
	  (start-process "dcd-server" "*dcd-server*" "dcd-server"))
    (set-process-query-on-exit-flag dcd-server-process nil)))

(defun dcd-call-with-buffer (out-buf &rest args)
  "Run the dcd client process and pass the current buffer as the last argument."
  (let ((buf (get-buffer-create out-buf))
	res)
    (with-current-buffer buf (erase-buffer))
    (setq res (apply 'call-process-region (point-min) (point-max)
		     dcd-client nil buf nil args))))

(defun dcd-parse-symbol-output ()
  "Parses the *dcd-temp* buffer and returns a list containing the file and point."
  (with-current-buffer "*dcd-temp*"
    (split-string (buffer-string))))

(defun dcd-jump-to-location (loc)
  "Jumps to the file/point in loc."
  (if (every 'string= loc '("Not" "found"))
      (message "Symbol not found")
    (let ((buffer (car loc))
	  (p (string-to-int (car (cdr loc)))))
      (if (string= buffer "stdin")
	  (progn (switch-to-buffer dcd-buffer)
		 (goto-char p))
	(progn (find-file buffer)
	       (goto-char p))))))

(defcustom dcd-keymap-prefix (kbd "C-c d")
  "dcd-mode keymap prefix."
  :type 'string)

(defvar dcd-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "c") 'dcd-complete)
      (define-key prefix-map (kbd ".") 'dcd-jump-to-symbol)
      (define-key prefix-map (kbd "d") 'dcd-symbol-doc)
      (define-key prefix-map (kbd "I") 'dcd-add-import-path)
      (define-key prefix-map (kbd "r") 'dcd-reset-server)
      (define-key map dcd-keymap-prefix prefix-map))
    map)
  "Keymap for dcd-mode.")

(defun dcd-action ()
  "Uses yasnippet to expand function arguments, when applicable."
  (let ((snippet (cdr (assoc (cdr ac-last-completion) dcd-fun-snippets))))
    (when snippet
      (yas/expand-snippet snippet dcd-point (point)))))

(defun dcd-complete ()
  "Get completion candidates from DCD."
  (unless dcd-init-complete
    (dcd-init))
  (setq dcd-buffer (current-buffer))
  (setq dcd-point (point))
  (setq dcd-fun-snippets '())
  (setq dcd-fun-help '())
  (unless (ac-in-string/comment)
    (save-restriction
      (widen)
      (apply 'dcd-call-with-buffer 
	     "*dcd-output*"
	     (append '("-c") 
		     (list (format "%s" (point)))))
      (dcd-parse-output "*dcd-output*"))))

;;;###autoload
(defun dcd-jump-to-symbol ()
  "Jump to the definition of the symbol at point."
  (interactive)
  (unless dcd-init-complete
    (dcd-init))
  (setq dcd-buffer (current-buffer))
  (setq dcd-point (point))
  (save-restriction
    (widen)
    (apply 'dcd-call-with-buffer 
	   "*dcd-temp*"
	   (append '("--symbolLocation")
		   '("-c") 
		   (list (format "%s" (point))))))
  (dcd-jump-to-location (dcd-parse-symbol-output)))

;;;###autoload
(defun dcd-symbol-doc ()
  "Show the documentation for the symbol at point."
  (interactive)
  (unless dcd-init-complete
    (dcd-init))
  (setq dcd-buffer (current-buffer))
  (setq dcd-point (point))
  (save-restriction
    (widen)
    (apply 'dcd-call-with-buffer 
	   "*dcd-doc*"
	   (append '("--doc")
		   '("-c") 
		   (list (format "%s" (point))))))
  (display-buffer "*dcd-doc*"))

;;;###autoload
(defun dcd-add-import-path (path)
  "Adds PATH to the list of import paths searched by DCD."
  (interactive "DImport Path: ")
  (unless dcd-init-complete
    (dcd-init))
  (let ((args (concat "-I" (file-truename path))))
    (call-process dcd-client nil nil nil args)))

;;;###autoload
(defun dcd-reset-server ()
  "Restarts the server manually."
  (when dcd-server-process
    (kill-process dcd-server-process)
    (dcd-init-server)))

(defun dcd-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (eq ?\. c)
          (point)))))

 (defun dcd-documentation (symbol)
   "Get documentation for the given symbol."
   (message (concat "requesting documentation for " symbol)))

(defvar ac-source-dcd
  '((candidates . dcd-complete)
    (requires . 0)
    (prefix . dcd-prefix)
    (document . dcd-documentation)
    (action . dcd-action)
))

;;;###autoload
(define-minor-mode dcd-mode
  "A minor mode to enable features of the DCD tool."
  :lighter " DCD"
  :keymap dcd-mode-map
  (when dcd-mode
    (unless dcd-init-complete
      (dcd-init))))

(provide 'dcd-mode)

;;; dcd-mode.el ends here
