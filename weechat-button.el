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

;;; Code:
;;

(require 'weechat)
(require 'button)

(defvar weechat-button-url-regexp
  (concat "\\(www\\.\\|\\(s?https?\\|"
          "ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)"
          "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
          "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,()]+[-a-zA-Z0-9_=#$@~`%&*+\\/()]")
  "Regexp to match URLs.
Copied from erc-button.el.")

(defun weechat-button--handler (button)
  "Handle BUTTON actions.
The function in property `weechat-function' gets called with `weechat-data'."
  (let ((function (button-get button 'weechat-function))
        (data (button-get button 'weechat-data)))
    (when function
      (funcall function data))))

(defun weechat-button--add ()
  "Add text buttons to text in buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward weechat-button-url-regexp nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (data (match-string 0)))
        (make-text-button start end
                          'action #'weechat-button--handler
                          'help-wecho "browse url"
                          'follow-link t
                          'weechat-function #'browse-url
                          'weechat-data data)))))

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
