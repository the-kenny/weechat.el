;;; weechat-sauron --- Sauron Notifications ;; -*- lexical-binding: t -*-

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

;; Notifications using Sauron https://github.com/djcb/sauron

;;; Code:

(require 'weechat)
(if (require 'sauron nil 'noerr)
    (progn
      (defun weechat-sauron-handler (type &optional sender text _date buffer-ptr)
        (setq text (if text (weechat-strip-formatting text)))
        (setq sender (if sender (weechat-strip-formatting sender)))
        (let ((jump-position (point-max-marker)))
          (sauron-add-event 'weechat 3
                            (cl-case type
                              (:highlight
                               (format "%s in %s: %S"
                                       sender
                                       (weechat-buffer-name buffer-ptr)
                                       text))
                              (:query
                               (format "Query from %s: %S"
                                       sender
                                       text))
                              (:disconnect
                               "Disconnected from WeeChat"))
                            (lambda ()
                              (when (fboundp 'sauron-switch-to-marker-or-buffer)
                                (sauron-switch-to-marker-or-buffer jump-position)))
                            ;; Flood protection based on sender
                            (when sender
                              (list :sender sender)))))

      (add-hook 'weechat-notification-handler-functions
                'weechat-sauron-handler))
  (weechat-warn "Error while loading weechat-sauron. Sauron notifications are disabled."))


(provide 'weechat-sauron)

;;; weechat-sauron.el ends here
