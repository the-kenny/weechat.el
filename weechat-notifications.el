;;; weechat-notifications --- notifications.el based notifications. ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

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

;; Notifications based on notifications.el.  Shipped with Emacs.

;;; Code:

(require 'weechat)
(require 'notifications)
(require 'xml) ;; For `xml-escape-string'

(defgroup weechat-notifications nil
  "Notifications based on notifications.el."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-notifications-"
  :group 'weechat)

(defcustom weechat-notifications-icon #'weechat-notifications-icon-function
  "Icon used in notifications.
Either nil, a file-name, or a function which is called with (SENDER BUFFER-PTR)."
  :type '(choice (const :tag "No icon" nil)
                 (file :tag "Icon file")
                 (function :tag "Icon function"))
  :group 'weechat-notifications)

(defcustom weechat-notifications-sound t
  "Sound to use for notifications:
- nil: No sound
- t: default message-new-instant sound
- string: file name of a sound file."
  :type '(choice (const :tag "No sound" nil)
                 (const :tag "Default system sound" t)
                 (file :tag "Sound file"))
  :group 'weechat-notifications)

(defun weechat-notifications-icon-function (_sender _buffer-ptr)
  "Default icon."
  (when (boundp 'notifications-application-icon)
    notifications-application-icon))

(defcustom weechat-notifications-libnotify-timeout 5000
  "Timeout for the libnotify notification (millseconds)"
  :group 'weechat-notifications)

(defcustom weechat-notifications-default-handler #'weechat-notifications-dbus-handler
  "The default notifications handler to be used as a hook to
weechat-notification-handler-functions. The two possible values are:
- #'weechat-nofifications-dbus-handler
- #'weechat-notifications-libnotify-handler"
  :type '(choice (const :tag "libnotify" #'weechat-notifications-libnotify-handler)
				 (const :tag "dbus" #'weechat-notifications-dbus-handler)
				 (function :tag "Notification Handler function"))
  :group 'weechat-notifications)

(defcustom weechat-notifications-libnotify-urgency "low"
  "Urgency for the notification send via libnotify
Values can be:
- low
- normal
- critical"
  :group 'weechat-notifications)

(defun notify-via-libnotify (title &optional body)
  "Send desktop notifications using libnotify."
  (call-process "notify-send" nil 0 nil
                title (if (eq body nil) "" body)
                "-t" (number-to-string weechat-notifications-libnotify-timeout)
                "-i" "emacs"
                "-u" weechat-notifications-libnotify-urgency
                "-c" "emacs.message"))

(defvar weechat--notifications-id-to-msg nil
  "Map notification ids to buffer-ptrs.")

(defun weechat--notifications-action (id key)
  "Handle notifcations.el actions.
See `weechat-notifications-handler'.

Supported actions:
- read: switch to buffer."
  (when (string= key "view")
    (let* ((buffer-ptr (cdr (assoc id weechat--notifications-id-to-msg))))
      (when buffer-ptr
        (weechat-switch-buffer buffer-ptr)))))

(defun weechat-notifications-get-title (type &optional sender)
  (cl-case type
    (:highlight
     (concat "Weechat.el: Message from <" sender ">"))
    (:query
     (concat "Weechat.el: Query from <" sender ">"))
    (:disconnect "Disconnected from WeeChat")
    (t " Weechat.el Alert")))

(defun weechat-notifications-dbus-handler (type &optional sender text _date buffer-ptr)
  "Notification handler using notifications.el."
  (let ((notifications-id
         (notifications-notify
          :title (weechat-notifications-get-title type (weechat-strip-formatting sender))
          :body (when text (xml-escape-string text))
          :category "im.received"
          :actions '("view" "View")
          :on-action #'weechat--notifications-action
          :app-icon (cl-typecase weechat-notifications-icon
                      (string weechat-notifications-icon)
                      (function (funcall weechat-notifications-icon
                                         sender buffer-ptr)))
          :app-name "WeeChat.el"
          :sound-name (when (and weechat-notifications-sound
                                 (not (stringp weechat-notifications-sound)))
                        "message-new-instant")
          :sound-file (when (stringp weechat-notifications-sound)
                        weechat-notifications-sound)
          :replaces-id (caar weechat--notifications-id-to-msg))))
    (when notifications-id
      (setq weechat--notifications-id-to-msg
            (append (list (cons notifications-id buffer-ptr))
                    weechat--notifications-id-to-msg)))))

(defun weechat-notifications-libnotify-handler (type &optional sender text _date buffer-ptr)
  "Notification handler using libnotify."
  (notify-via-libnotify
   (weechat-notifications-get-title type sender)
   (if (eq text nil) nil (xml-escape-string text))))

(add-hook 'weechat-notification-handler-functions
          #'weechat-notifications-default-handler)

(provide 'weechat-notifications)

;;; weechat-notifications.el ends here
