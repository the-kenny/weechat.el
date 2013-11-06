;;; weechat --- Chat via Weechat's relay protocol in Emacs ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Keywords: irc
;; URL: https://github.com/the-kenny/weechat.el

;; This file is NOT part of GNU Emacs.

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

;; This code is inspired by erc-speedbar.el

;;; Code:

(require 'weechat)
(require 'speedbar)

(defvar weechat-speedbar-key-map
  (let ((map (speedbar-make-specialized-keymap)))
    (define-key map "e" 'speedbar-edit-line)
    (define-key map "\C-m" 'speedbar-edit-line)
    (define-key map "+" 'speedbar-expand-line)
    (define-key map "=" 'speedbar-expand-line)
    (define-key map "-" 'speedbar-contract-line)
    map)
  "Weechat Speedbar Key Map")

(defvar weechat-speedbar-menu-items
  '(["Go to buffer" speedbar-edit-line t]
    ["Expand Node" speedbat-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Node" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))])
  "Weechat Speedbar Menu Items")

(defun weechat-speedbar--goto-buffer (_text buffer _indent)
  (let ((buf (get-buffer buffer)))
    (speedbar-set-timer dframe-update-speed)
    (if (buffer-live-p buf)
        (switch-to-buffer buf)
      (when (y-or-n-p (format "Monitor buffer '%s'? " buffer))
        (weechat-monitor-buffer (weechat--find-buffer buffer) 'show)))))

(defvar weechat--buffer-hashes) ;; See weechat.el

(defun weechat-speedbar--buttons (_directory depth)
  "Create buttons for speedbar in BUFFER."
  (erase-buffer)
  (maphash
   #'(lambda (_k v)
       (let* ((local-vars (gethash "local_variables" v))
              (type (cdr (assoc-string "type" local-vars)))
              (name (cdr (assoc-string "name" local-vars)))
              (server (cdr (assoc-string "server" local-vars))))
         (when (string= type "server")
           (speedbar-with-writable
             (speedbar-make-tag-line
              'bracket ?+
              #'weechat-speedbar--expand-server server
              server
              #'weechat-speedbar--goto-buffer name
              nil depth)))))
   weechat--buffer-hashes))

(defun weechat-speedbar--expand-server (text server indent)
  (cond ((string-match "+" text) ; expand node
         (speedbar-change-expand-button-char ?-)
         (speedbar-reset-scanners)
         (speedbar-with-writable
           (save-excursion
             (forward-line 1)
             (weechat-speedbar--channel-buttons nil (1+ indent) server))))
        ((string-match "-" text) ; contract node
         (speedbar-change-expand-button-char ?+)
         (speedbar-delete-subblock indent))
        (t (error "Not sure what do do!")))
  (speedbar-center-buffer-smartly))

(defun weechat-speedbar--channel-buttons (_directory depth for-server)
  (maphash
   #'(lambda (_k v)
       (let* ((local-vars (gethash "local_variables" v))
              (type (cdr (assoc-string "type" local-vars)))
              (name (cdr (assoc-string "name" local-vars)))
              (channel (cdr (assoc-string "channel" local-vars)))
              (server (cdr (assoc-string "server" local-vars))))
         (when (string= server for-server)
           (cond
            ((string= type "channel")
             (speedbar-with-writable
               (speedbar-make-tag-line
                'bracket ?+
                #'weechat-speedbar--expand-channel name
                channel
                #'weechat-speedbar--goto-buffer name
                nil depth)))
            ((string= type "private")
             (speedbar-with-writable
               (speedbar-with-writable
                 (speedbar-make-tag-line ;; TODO org-contacts/gravatar/bbdb?
                  nil nil
                  nil nil ;; TODO expand for whois?
                  channel
                  #'weechat-speedbar--goto-buffer name
                  nil depth))))))))
   weechat--buffer-hashes))

(defun weechat-speedbar--expand-channel (text name indent)
   (cond ((string-match "+" text) ; expand node
         (speedbar-change-expand-button-char ?-)
         (speedbar-reset-scanners)
         (speedbar-with-writable
           (save-excursion
             (forward-line 1)
             (weechat-speedbar--user-buttons nil (1+ indent) name))))
        ((string-match "-" text) ; contract node
         (speedbar-change-expand-button-char ?+)
         (speedbar-delete-subblock indent))
        (t (error "Not sure what do do!")))
   (speedbar-center-buffer-smartly))

(defvar weechat-user-list) ;; See weechat.el
(defun weechat-speedbar--user-buttons (_directory depth name)
  (let ((buffer (get-buffer name))
        nick-list
        topic)
    (if (not (buffer-live-p buffer))
        (speedbar-with-writable
          (speedbar-make-tag-line
           'angle ?i
           nil nil
           "Not Monitored!"
           #'weechat-speedbar--goto-buffer name
           nil depth))
      (with-current-buffer buffer
        (setq nick-list weechat-user-list)
        (setq topic weechat-topic))
      (when topic
        (speedbar-with-writable
          (speedbar-make-tag-line
           'angle ?i
           nil nil
           (concat "Topic: " topic)
           nil nil
           nil depth)))
      (dolist (nick nick-list)
        (speedbar-with-writable
         (speedbar-make-tag-line ;; TODO org-contacts/gravatar/bbdb?
          nil nil
          nil nil ;; TODO expand for whois?
          nick
          #'weechat-speedbar--user-action nick
          nil depth))))))

(defun weechat-speedbar--user-action (_text nick _indent)
  (weechat-nick-action nick))

(defun weechat-speedbar-item-info ()
  "Display information about the current line."
  (let ((data (speedbar-line-token)))
    (message "item-info: %s" data)))

(defvar weechat-speedbar--to-do-point t
  "Local variable maintaining the current modified check position.")

(defcustom weechat-speedbar-highlight-indicator "<i>"
  "Indicator used for highlight events."
  :type 'string
  :group 'weechat-speedbar)

(defcustom weechat-speedbar-modified-indicator "<M>"
  "Indicator used for normal message events."
  :type 'string
  :group 'weechat-speedbar)

(defun weechat-speedbar--add-indicator (indicator)
  "Add INDICATOR to current line.
This is similar to `speedbar-add-indicator' but supports weechat-speedbar
indicators."
  (save-excursion
    (beginning-of-line)
    (end-of-line)
    (when (re-search-backward (concat "\\("
                                      (regexp-quote weechat-speedbar-highlight-indicator)
                                      "\\|"
                                      (regexp-quote weechat-speedbar-modified-indicator)
                                      "\\)+")
                              (line-beginning-position) t)
      (delete-region (match-beginning 0) (match-end 0)))
    (end-of-line)
    (when (not (string= " " indicator))
      (let ((start (point)))
        (speedbar-with-writable
          (insert indicator)
          (speedbar-insert-image-button-maybe start (length indicator)))))))

(defun weechat-speedbar--check-modified ()
  "Scan all the channels and check if they are modified."
  (save-excursion
    (when speedbar-buffer
      (set-buffer speedbar-buffer))
    (when (eq weechat-speedbar--to-do-point t)
      (setq weechat-speedbar--to-do-point 0))
    (if (not (numberp weechat-speedbar--to-do-point))
        t
      (goto-char weechat-speedbar--to-do-point)
      (while (and (not (input-pending-p))
                  (re-search-forward "^1: \\(\\[[+-]\\]\\|>\\) " nil t))
        (setq weechat-speedbar--to-do-point (point))
        (let* ((buffer-name (get-text-property (point) 'speedbar-token))
               (buffer-ptr (weechat--find-buffer buffer-name)))
          (weechat-speedbar--add-indicator
           (if buffer-ptr
               (let ((hash (weechat-buffer-hash buffer-ptr)))
                (cond
                 ((gethash :background-highlight hash)
                  weechat-speedbar-highlight-indicator)
                 ((gethash :background-message hash)
                  weechat-speedbar-modified-indicator)
                 (t " ")))
             " "))))
      (if (input-pending-p)
          nil                                    ; we are incomplete
        (setq weechat-speedbar--to-do-point nil) ; done
        t))))

(defun weechat-speedbar-install-variables ()
  "Install WeeChat speedbar variables."
  (speedbar-add-expansion-list '("WeeChat"
                                 weechat-speedbar-menu-items
                                 weechat-speedbar-key-map
                                 weechat-speedbar--buttons))
  (add-to-list 'speedbar-stealthy-function-list
               '("WeeChat" weechat-speedbar--check-modified))
  (add-hook 'speedbar-scanner-reset-hook
            (lambda ()
              (setq weechat-speedbar--to-do-point t)))
  (speedbar-add-mode-functions-list
   '("WeeChat" (speedbar-item-info . weechat-speedbar-item-info))))

(if (featurep 'speedbar)
    (weechat-speedbar-install-variables)
  (add-hook 'speedbar-load-hook #'weechat-speedbar-install-variables))

(defun weechat-speedbar-unload-function ()
  ;; TODO
  )

(provide 'weechat-speedbar)

;;; weechat-speedbar.el ends here
