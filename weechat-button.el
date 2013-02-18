;;; weechat-button --- Add buttons to text ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Keywords: irc chat network weechat
;; URL: https://github.com/the-kenny/weechat.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This code is heavily inspired by erc-button.el!

;;; Code:
;;

(require 'weechat)
(require 'button)

;;; Customize

(defgroup weechat-button nil
  "WeeChat button interface (URLification)."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-button"
  :group 'weechat)

(defcustom weechat-button-url-regexp
  (concat "\\(www\\.\\|\\(s?https?\\|"
          "ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)"
          "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
          "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,()]+[-a-zA-Z0-9_=#$@~`%&*+\\/()]")
  "Regexp to match URLs.
Copied from erc-button.el."
  :type 'regexp
  :group 'weechat-button)

(defcustom weechat-button-default-log-buffer "*WeeChat URL Log*"
  "Buffer name for URL log."
  :group 'weechat-button
  :type 'string)

(defcustom weechat-button-list
  '((weechat-button-url-regexp 0 t t "Browse URL" browse-url 0)
    ("[`]\\([-_.[:alnum:]]+\\)[']" 1 t nil "Describe Symbol"
     weechat-button--describe-symbol 1))
  "List of potential buttons in WeeChat chat buffers.
Each entry has the form (REGEXP BUTTON-MATCH BUTTONIZE? LOG HELP-ECHO ACTION
DATA-MATCH...), where

REGEXP is a string or variable containing a regular expression to match buttons.

BUTTON-MATCH is the number of the regexp grouping which represents the actual
  button.

BUTTONIZE? if t the button is always created if it is a function then the button
  is only created if it evals to non-nil.

LOG decides if `weechat-button-log-functions' gets called.

HELP-ECHO is the `help-echo' property of the button.
  See Info node `(elisp) Button Properties'.

ACTION the function to call when the button is selected.

DATA-MATCH... numbers of the regexp groupings whose text will be passed to
  ACTION.

This is similar (but not identical) to `erc-button-alist' in ERC."
  :group 'weechat-button
  :type '(repeat :tag "Buttons"
                 (list (choice :tag "Matches"
                               regexp
                               (variable :tag "Variable containing regexp"))
                       (integer :tag "Number of the regexp section that matches")
                       (choice :tag "When to buttonize"
                               (const :tag "Always" t)
                               (function :tag "Only when the function returns non-nil"))
                       (choice :tag "Log match"
                               (const :tag "To default buffer" t)
                               (const :tag "Never" nil)
                               (string :tag "To buffer name"))
                       (string :tag "Help echo text")
                       (function :tag "Call this function when button is pressed")
                       (repeat :tag "Sections of regexp to send to the function"
                               :inline t
                               (integer :tag "Regexp section number")))))

(defvar weechat-button-log-functions nil
  "List of function to run when a button should be logged.

This hook only runs when `LOG' is set to `t' for the particular
button type.

Functions in list must have two arguments: The button data (the
match string) and a plist describing the button properties.")

;;; Internal functions

(defun weechat-button--handler (button)
  "Handle BUTTON actions.
The function in property `weechat-function' gets called with `weechat-data'."
  (let ((function (button-get button 'weechat-function))
        (data (button-get button 'weechat-data)))
    (when function
      (apply function data))))

(defun weechat-button--insert-log (button-data button-properties)
  (let ((weechat-buffer-name (buffer-name)))
    (with-current-buffer (get-buffer-create
                          weechat-button-default-log-buffer)
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert weechat-buffer-name "\t")
      (apply #'insert-text-button button-data button-properties)
      (insert "\n"))))

(add-hook 'weechat-button-log-functions 'weechat-button--insert-log)

(defun weechat-button--add-do (entry)
  "Handle each button ENTRY."
  (save-excursion
    (goto-char (point-min))
    (let* ((regexp-entry (nth 0 entry))
           (regexp (or (and (stringp regexp-entry) regexp-entry)
                       (and (boundp regexp-entry) (symbol-value regexp-entry))))
           (button-match (nth 1 entry))
           (buttonize? (nth 2 entry))
           (log (nth 3 entry))
           (help-echo (nth 4 entry))
           (action (nth 5 entry))
           (data-match (nthcdr 6 entry)))
      (when regexp
        (while (re-search-forward regexp nil t)
          (let ((start (match-beginning button-match))
                (end (match-end button-match))
                (button-data (match-string button-match))
                (data (mapcar #'match-string data-match)))
            (when (or (eq buttonize? t)
                      (and (functionp buttonize?)
                           (funcall buttonize?)))
              (let ((properties (list 'action #'weechat-button--handler
                                      'help-echo help-echo
                                      'follow-link t
                                      'weechat-function action
                                      'weechat-data data)))
                (when log
                  ;; Hack: Rebind `weechat-button-default-log-buffer'
                  ;; to the value supplied by the button type in
                  ;; `weechat-button-list'
                  (let ((weechat-button-default-log-buffer
                         (if (or (stringp log) (bufferp log))
                             log
                           weechat-button-default-log-buffer)))
                    (run-hook-with-args 'weechat-button-log-functions
                                        button-data
                                        properties)))
                (apply #'make-button start end properties)))))))))

(defun weechat-button--add ()
  "Add text buttons to text in buffer."
  (dolist (i weechat-button-list)
    (weechat-button--add-do i)))

;;; Callback functions

;; This function is copied from `erc-button-describe-symbol'
(defun weechat-button--describe-symbol (symbol-name)
  "Describe SYMBOL-NAME.
Use `describe-function' for functions, `describe-variable' for variables,
and `apropos' for other symbols."
  (let ((symbol (intern-soft symbol-name)))
    (cond ((and symbol (fboundp symbol))
           (describe-function symbol))
          ((and symbol (boundp symbol))
           (describe-variable symbol))
          (t (apropos symbol-name)))))

;;; Module load/unload

;; TODO module system
(defun weechat-button-enable ()
  "Enable module."
  (add-hook 'weechat-insert-modify-hook
            #'weechat-button--add))

(defun weechat-button-disable ()
  "Disable module."
  (remove-hook 'weechat-insert-modify-hook
               #'weechat-button--add))


(provide 'weechat-button)

;;; weechat-button.el ends here
