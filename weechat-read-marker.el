;;; weechat-read-marker.el --- read marker for WeeChat  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Hans-Peter Deifel

;; Author: Hans-Peter Deifel <hpd@hpdeifel.de>
;; Created: 22 Jun 2015

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Draw a thin red line between the read and unread lines in a buffer.

;;; See `M-x customize-group weechat-read-marker' for configuration options.

;;; If anything goes wrong, the read marker can be manually reset with
;;; `weechat-read-marker-reset'.

;;; Code:

(require 'weechat)

(defgroup weechat-read-marker nil
  "Read marker for WeeChat"
  :prefix "weechat-read-marker"
  :group 'weechat)

(defcustom weechat-read-marker-char ?─
  "Character used to render the read marker."
  :type 'character
  :group 'weechat-read-marker)

(defface weechat-read-marker
  '((t :foreground "brown"))
  "Face used to colorize the read marker."
  :group 'weechat-read-marker)

(defcustom weechat-read-marker-always-show nil
  "Always show read marker, even if it is after last buffer line.

A value of t means that the read marker is displayed directly
over the prompt, if there are no unread lines.  With nil, the
marker is simply not displayed in this case."
  :type 'boolean
  :group 'weechat-read-marker)

(defvar-local weechat-read-marker-overlay nil
  "The overlay used to draw the read marker.

Will be nil initially and if the buffer has no unread lines when
`weechat-read-marker-always-show` is not set.")

(defvar-local weechat-read-marker-stale t
  "Whether the read marker position is outdated.

This will be set if the buffer is visited, to indicate that the
unread lines are now read.")

(defun weechat-read-marker--set-overlay ()
  "Update the `after-string' property on an already existing overlay."
  (let* ((width (- (window-width) 1))
         (line (make-string width weechat-read-marker-char)))
    (overlay-put weechat-read-marker-overlay 'after-string
                 (concat "\n" (propertize line 'face 'weechat-read-marker)))))

(defun weechat-read-marker--move-overlay ()
  "Update the read marker in the current buffer."
  (if weechat-read-marker-overlay
      (move-overlay weechat-read-marker-overlay (point-at-bol) (point-at-eol))
    (setq weechat-read-marker-overlay
          (make-overlay (point-at-bol) (point-at-eol) nil t nil))
    ;; Delete overlay if text is deleted. This is needed to get rid of the
    ;; overlay, when the buffer is reset.
    (overlay-put weechat-read-marker-overlay 'evaporate t))

  (weechat-read-marker--set-overlay))

(defun weechat-read-marker-handle-visited ()
  "Reset read marker after a buffer is being visited."
  (if weechat-read-marker-stale
      ;; we haven't had any new lines. Reset overlay
      (when weechat-read-marker-overlay
        (if weechat-read-marker-always-show
            (save-excursion
              (goto-char (point-max))
              (forward-line -1)
              (weechat-read-marker--move-overlay))
          (delete-overlay weechat-read-marker-overlay)
          (setq weechat-read-marker-overlay nil)))
    ;; otherwise, set the read marker to be stale
    (setq weechat-read-marker-stale t)
    ;; and also recompute the overlay string, since the window-width could have
    ;; changed
    (when weechat-read-marker-overlay
      (weechat-read-marker--set-overlay))))

(defun weechat-read-marker-handle-background ()
  "Move read-marker in case it is stale."
  (when weechat-read-marker-stale
    (save-excursion
      (goto-char (point-max))
      (unless (< (line-number-at-pos) 3)
        (forward-line -2)
        (weechat-read-marker--move-overlay)
        (setq weechat-read-marker-stale nil)))))

(defun weechat-read-marker-reset ()
  "Manually reset the read marker in the current buffer."
  (interactive "")
  (when weechat-read-marker-overlay
    ;; Always delete (and possibly) recreate overlay in case anything went wrong
    ;; and the users used this command to reset things.
    (delete-overlay weechat-read-marker-overlay)
    (if weechat-read-marker-always-show
        (save-excursion
          (goto-char (point-max))
          (forward-line -1)
          (weechat-read-marker--set-overlay))
      (setq weechat-read-marker-overlay nil)))
  (setq weechat-read-marker-stale t))


(add-hook 'weechat-buffer-background-message-hook
          'weechat-read-marker-handle-background)
(add-hook 'weechat-buffer-visited-hook
          'weechat-read-marker-handle-visited)

(provide 'weechat-read-marker)

;;; weechat-read-marker.el ends here
