;;; weechat --- Support secrets.el -*- lexical-binding: t -*-

;; Copyright (C) 2013  Rüdiger Sonderfeld

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

;; secrets.el support for weechat.el

;; Load this file and then set `weechat-password-callback' to
;; `weechat-secrets-get-password'.  You can use
;; `weechat-secrets-create' to create new entries and
;; `weechat-secrets-delete' to delete them.

;;; Code:

(require 'weechat)
(require 'secrets)

(defgroup weechat-secrets nil
  "Secrets.el support for WeeChat."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-secrets"
  :group 'weechat)

(defcustom weechat-secrets-collection "weechat.el"
  "Collection name."
  :type 'string
  :group 'weechat-secrets)

(defun weechat-secrets--to-item (host port)
  "Convert HOST and PORT to an item name."
  (format "%s:%s" host port))

(defun weechat-secrets-create (host port &optional password)
  "Associate HOST and PORT with PASSWORD.
A collection named after `weechat-secrets-collection' is created if required."
  (interactive
   (list
    (read-string
     (format "Host for password (default '%s'): " weechat-host-default)
     nil nil weechat-host-default)
    (read-number "Port for password: " weechat-port-default)))
  (unless secrets-enabled
    (error "Secrets.el-API not available."))
  (unless (member weechat-secrets-collection (secrets-list-collections))
    (secrets-create-collection weechat-secrets-collection))
  (unless password
    (setq password (read-passwd "Password: " 'confirm)))
  (secrets-create-item weechat-secrets-collection
                       (weechat-secrets--to-item host port) password
                       :host host :port (number-to-string port))
  (clear-string password))

(defun weechat-secrets-delete (host port &optional allow-empty-collection)
  "Delete HOST and PORT entry.
Unless ALLOW-EMPTY-COLLECTION is non-nil then the collection is removed
if it is empty."
  (interactive
   (list
    (read-string
     (format "Host for password (default '%s'): " weechat-host-default)
     nil nil weechat-host-default)
    (read-number "Port for password: " weechat-port-default)))
  (let ((item (weechat-secrets--to-item host port)))
    (secrets-delete-item weechat-secrets-collection item)
    (unless (and allow-empty-collection
                 (null (secrets-list-items weechat-secrets-collection)))
      (secrets-delete-collection weechat-secrets-collection))))

(defun weechat-secrets-get-password (host port)
  "Get password for HOST and PORT."
  (secrets-get-secret weechat-secrets-collection
                      (weechat-secrets--to-item host port)))

(provide 'weechat-secrets)

;;; weechat-secrets.el ends here
