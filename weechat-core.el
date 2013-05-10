;;; weechat-core --- Basic stuff for weechat.el ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Moritz Ulrich

;; Author: Moritz Ulrich <moritz@tarn-vedra.de>
;;         Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;;         Aristid Breitkreuz <aristidb@gmail.com>
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

;; This package provides a way to chat via WeeChat's relay protocol in
;; Emacs.

;; Please see README.org on how to use it.

;;; Code:

(require 'cl-lib)
(require 's)

(defgroup weechat nil
  "WeeChat based IRC client for Emacs."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-"
  :group 'applications)

(defgroup weechat-relay nil
  "WeeChat Relay Settings."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-relay"
  :group 'weechat)

(defcustom weechat-relay-log-buffer-name "*weechat-relay-log*"
  "Buffer name to use as debug log.
Set to nil to disable logging."
  :type 'string
  :group 'weechat-relay)

(defcustom weechat-relay-log-level :info
  "Minimum log level.
Might be one of :debug, :info, :warn, :error or nil."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Error" :error)
                 (const :tag "Warnings" :warn)
                 (const :tag "Info" :info)
                 (const :tag "Debug" :debug))
  :group 'weechat-relay)

(defun weechat-relay-log (text &optional level)
  "Log `TEXT' to `weechat-relay-log-buffer-name' if enabled.
`LEVEL' might be one of :debug :info :warn :error.  Defaults
to :info"
  (let ((log-level-alist '((:debug . 0)
                           (:info  . 1)
                           (:warn  . 2)
                           (:error . 3))))
    (when (and (>= (assoc-default (or level :info) log-level-alist)
                   (assoc-default weechat-relay-log-level log-level-alist))
               weechat-relay-log-level
               weechat-relay-log-buffer-name)
      (with-current-buffer (get-buffer-create weechat-relay-log-buffer-name)
        (let ((old-point (point)))
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-max))
              (insert (s-trim text))
              (newline)))
          (goto-char old-point))))))

(defun weechat-warn (message &rest args)
  "Display MESSAGE with `warn' and log it to `weechat-relay-log-buffer-name'."
  (let ((str (apply 'format message args)))
    (weechat-relay-log str :warn)
    (display-warning 'weechat str)))

(defun weechat-message (format-string &rest args)
  "Log MESSAGE with log-level :info and call `message'."
  (let ((text (apply 'format format-string args)))
    (weechat-relay-log text :info)
    (message text)))

(provide 'weechat-core)

;;; weechat-core.el ends here
