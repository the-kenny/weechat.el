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
    (if (buffer-live-p buf)
        (switch-to-buffer buf)
      (when (y-or-n-p (format "Monitor buffer '%s'? " buffer))
        (weechat-monitor-buffer buffer 'show)))))

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
           (if (string= type "channel")
               (speedbar-with-writable
                 (speedbar-make-tag-line
                  'bracket ?+
                  #'weechat-speedbar--expand-channel name
                  channel
                  #'weechat-speedbar--goto-buffer name
                  nil depth))
             (when (string= type "private")
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
        nick-list)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq nick-list weechat-user-list))
      (dolist (nick nick-list)
        (speedbar-with-writable
         (speedbar-make-tag-line ;; TODO org-contacts/gravatar/bbdb?
          nil nil
          nil nil ;; TODO expand for whois?
          nick
          nil nil ;; TODO query? whois?
          nil depth))))))

(defun weechat-speedbar-item-info ()
  "Display information about the current line."
  (let ((data (speedbar-line-token)))
    (message "item-info: %s" data)))

(defun weechat-speedbar-install-variables ()
  "Install WeeChat speedbar variables."
  (speedbar-add-expansion-list '("WeeChat"
                                 weechat-speedbar-menu-items
                                 weechat-speedbar-key-map
                                 weechat-speedbar--buttons))
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
