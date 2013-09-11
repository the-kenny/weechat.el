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

(defvar weechat-awareness-buffer-name "*weechat-awareness*")

(setq weechat-awareness-name-length 16)

(defvar weechat-awareness-show-unread t)

(defun weechat-awareness-indicator-string (buffer-ptr)
  (if (= 0 (random 2))
      (format "(%i)" (random 42))
    (format "(%i,%i)" (random 42) (random 42))))

(defun weechat-awareness-make-cell (buffer-ptr pad)
  (let ((bname (s-truncate weechat-awareness-name-length
                           (weechat-canonical-buffer-name buffer-ptr)))
        (indicator (if weechat-awareness-show-unread
                       (weechat-awareness-indicator-string buffer-ptr)
                     "")))
    (propertize
     (s-pad-right
      pad
      " "
      (format " %s%s%s"
              bname
              (make-string (- pad
                              (length bname)
                              (length indicator)
                              2)
                           32)
              indicator))
     'weechat-awareness-buffer-ptr buffer-ptr)))

(defun weechat-awareness-update-buffer (buffer)
  (with-current-buffer buffer
    (let* ((pos (point))
           (weechat-buffers (weechat-buffer-list))
           (weechat-awareness-max-column-width (+ weechat-awareness-name-length
                                                  (if weechat-awareness-show-unread
                                                      6 0)
                                                  2))
           (_ (message "%S" weechat-awareness-max-column-width))
           (columns (/ (window-width)
                       (+ 2 weechat-awareness-max-column-width)))
           (rows (ceiling
                  (/ (length weechat-buffers)
                     (float columns)))))
      (let ((inhibit-read-only t))
        (kill-region (point-min) (point-max))
        (goto-char (point-min))
        (org-table-create (format "%ix%i" columns rows))
        (org-table-begin)
        (when (> rows 1)
          (next-line)
          (kill-region (point-at-bol) (1+ (point-at-eol)))
          (org-table-begin))
        (cl-dotimes (x columns)
          (cl-dotimes (y rows)
            (let ((buffer (nth (+ (* y columns) x)
                               weechat-buffers)))
              (when (bufferp buffer)
                (let ((buffer-ptr (buffer-local-value
                                   'weechat-buffer-ptr buffer)))
                  (org-table-put
                   (1+ y) (1+ x)
                   (weechat-awareness-make-cell buffer-ptr weechat-awareness-max-column-width)))))))
        ;; Fill the last cells in the table
        (when (< (length weechat-buffers) (* columns rows))
          (cl-dotimes (i (- (* columns rows) (length weechat-buffers)))
            (org-table-put rows (- columns i)
                           (s-pad-right weechat-awareness-max-column-width
                                        " " "")))))
      (when (window-live-p (get-buffer-window buffer))
        (set-window-point (get-buffer-window buffer) pos)))))

(defun weechat-awareness-next (pos)
  (interactive "d")
  (if (org-table-p)
      (if (= pos (org-table-begin))
          (goto-char (1+ (point)))
        (let ((org-table-automatic-realign nil))
          (org-table-next-field)))
    (goto-char (1+ (point-min)))))

(defun weechat-awareness-prev (pos)
  (interactive "d")
  (if (org-table-p)
      (if (= (org-table-begin) pos)
          (goto-char (1- (org-table-end)))
        (let ((org-table-automatic-realign nil))
          (org-table-previous-field)))
    (let ((p (save-excursion
               (goto-char (1+ (point-min)))
               (org-table-end))))
      (goto-char (1- p)))))

(defun weechat-awareness-tab (pos)
  (interactive "d"))

(defun weechat-awareness-goto (pos)
  (interactive "d")
  (let ((buffer-ptr (get-text-property
                     (if (and (> pos 1) (equal "|" (buffer-substring pos (1+ pos))))
                         (1- pos) pos)
                     'weechat-awareness-buffer-ptr)))
    (when (weechat-buffer-hash buffer-ptr)
      (let ((buf (weechat--emacs-buffer buffer-ptr)))
        (if (bufferp buf)
            (switch-to-buffer buf)
          (weechat-monitor-buffer buffer-ptr 'show))))))

(defvar weechat-awareness-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'weechat-awareness-goto)
    (define-key map (kbd "p")   'weechat-awareness-prev)
    (define-key map (kbd "n")   'weechat-awareness-next)
    (define-key map (kbd "TAB") 'weechat-awareness-tab)
    (define-key map (kbd "r")   'weechat-awareness-reload)
    map)
  "Keymap for weechat.el's awareness plugin.")

(defun weechat-awareness-mode ()
  (setq major-mode 'weechat-awareness
        mode-name "Weechat-Awareness")
  (use-local-map weechat-awareness-mode-map)
  (read-only-mode 1))

(defun weechat-awareness ()
  (interactive)
  (let ((buffer (get-buffer-create
                 weechat-awareness-buffer-name)))
    (with-current-buffer buffer
      (weechat-awareness-mode)
      (weechat-awareness-update-buffer buffer))))

(provide 'weechat-awareness)

;;; weechat-awareness.el ends here
