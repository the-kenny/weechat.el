;;; weechat-awareness.el --- Get an overview of all weechat.el activities

;; Copyright (C) 2013 Moritz Ulrich <moritz@tarn-vedra.de>

;; Author: Moritz Ulrich <moritz@tarn-vedra.de>
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

(require 'weechat)

(defvar weechat-awareness-max-column-width 18)

(let* ((weechat-buffers (weechat-buffer-list))
       (columns (/ (window-width)
                   (+ weechat-awareness-max-column-width 3)))
       (rows (ceiling
              (/ (length weechat-buffers)
                 (float columns)))))
  (with-current-buffer (get-buffer "tabletest")
    (let ((inhibit-read-only t))
      (kill-region (point-min) (point-max)))
    (goto-char (point-min))
    (org-table-create (format "%ix%i" columns rows))
    (org-table-begin)
    (next-line)
    (kill-region (point-at-bol) (1+ (point-at-eol)))
    (org-table-begin)
    (cl-dotimes (x columns)
      (cl-dotimes (y rows)
        (let ((buffer (nth (+ (* y columns) x)
                           weechat-buffers)))
          (when (bufferp buffer)
            (org-table-put
             (1+ y) (1+ x)
             (s-pad-right
              weechat-awareness-max-column-width " "
              (s-prepend " "
                         (weechat-canonical-buffer-name
                          (buffer-local-value
                           'weechat-buffer-ptr buffer)))))))))
    ;; Fill the last cells in the table
    (when (< (length weechat-buffers) (* columns rows))
      (cl-dotimes (i (- (* columns rows) (length weechat-buffers)))
        (org-table-put rows (- columns i)
                       (s-pad-right weechat-awareness-max-column-width " " ""))))
    (add-text-properties (point-min) (point-max)
                         '(read-only t))))

(provide 'weechat-awareness)

;;; weechat-awareness.el ends here
