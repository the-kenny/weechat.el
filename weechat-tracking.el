;;; weechat-tracking --- Tracking support for weechat.el ;; -*- lexical-binding: t -*-

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

;; This module provides tracking support, similar to erc-track, for weechat.el.
;; It requires the tracking library
;; https://github.com/jorgenschaefer/circe/wiki/Tracking
;; It should be available from marmalade or el-get.

;;; Code:

(require 'tracking)

(defgroup weechat-tracking nil
  "Tracking support for Weechat.el."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-tracking-"
  :group 'weechat)

(defcustom weechat-tracking-faces-priorities '(weechat-highlight-face) ;; TODO
  "A list of faces which should show up in the tracking.
The first face is kept if the new message has only lower faces,
or faces that don't show up at all."
  :type '(repeat face)
  :group 'weechat-tracking)

(defun weechat-tracking-setup ()
  "Set up tracking in weechat buffer."
  (set (make-local-variable 'tracking-faces-priorities) weechat-faces-priorities))

(weechat-do-buffers (weechat-tracking-setup))
(add-hook 'weechat-mode-hook #'weechat-tracking-setup)

(provide 'weechat-tracking)

;; weechat-tracking.el ends here

