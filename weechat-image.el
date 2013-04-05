;;; weechat-image --- Image preview ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;;         Moritz Ulrich <moritz@tarn-vedra.de>
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
;;
;; TODO: resize, make buffer more beautiful, test test test

;;; Code:
;;

(require 'weechat)

;;; Customize:
;;

(defgroup weechat-image nil
  "Image previews for WeeChat."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-image"
  :group 'weechat)

(defcustom weechat-image-url-regex "\\.\\(png\\|jpe?g\\|gif\\|svg\\)"
  "Regexp to match image URLs.
This gets called on a URL matched with `thing-at-point' and `url'."
  :type 'regexp
  :group 'weechat-image)

(defcustom weechat-image-url-blacklist-regex "/\\(Datei\\|File\\):"
  "Blacklist for image URLs.
E.g., for Wikipedia links starting with File:.  They do not link directly to the image."
  :type 'regexp
  :group 'weechat-image)

(defcustom weechat-image-display-func #'weechat-image-insert-inline
  "Function to call to insert image.
The Function should accept the following paramter (URL IMAGE BUFFER MARKER)."
  :type '(choice (const :tag "Inline" weechat-image-insert-inline)
                 (const :tag "Other Buffer" weechat-image-insert-other-buffer)
                 (function :tag "Call function"))
  :group 'weechat-image)

(defcustom weechat-image-buffer "*WeeChat Image Buffer*"
  "Buffer used if `weechat-image-display-func' is set to ``Other Buffer''."
  :type 'string
  :group 'weechat-image)

(defcustom weechat-image-use-imagemagick (fboundp 'imagemagick-types)
  ;; TODO is there a better way to identify if emacs has imagemagick support?
  "Use ImageMagick to load images."
  :type 'boolean
  :group 'weechat-image)

(defcustom weechat-image-size-limit (* 1024 1024) ;; 1M
  "Size limit for images."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Size limit in bytes"))
  :group 'weechat-image)

(defcustom weechat-image-max-width (/ (frame-pixel-width nil) 2)
  "Max image width.
If `weechate-image-size' is non-nil the image is resized.  Be aware that
`weechat-image-size-limit' is checked before."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max width in pixel"))
  :group 'weechat-image)

(defcustom weechat-image-max-height nil
  "Max image height.
If `weechate-image-size' is non-nil the image is resized.  Be aware that
`weechat-image-size-limit' is checked before."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max height in pixel"))
  :group 'weechat-image)

(defcustom weechat-image-resize weechat-image-use-imagemagick
  "Resize image if it's larger than `weechat-image-max-width' and
`weechat-image-max-height'.  This only works if imagemagick is used.
See `weechat-image-use-imagemagick'."
  :type 'boolean
  :group 'weechat-image)

(defcustom weechat-image-time-format "%Y-%m-%dT%T%z" ;; ISO 8601
  "Timestamp format used in `weechat-image-buffer'.
See `format-time-string'."
  :type 'string
  :group 'weechat-image)

(defun weechat-image--remove (button)
  "Remove image associated with BUTTON."
  (let ((start (button-get button 'weechat-image-begin))
        (end (button-get button 'weechat-image-end)))
    (remove-images start end)
    (delete-region (1- (overlay-start button)) (overlay-end button))
    (delete-overlay button)
    (save-excursion
      (save-restriction
        (narrow-to-region (line-beginning-position) (line-end-position))
        (let ((inhibit-read-only t))
          (weechat-image--add-preview-button))))))

(defun weechat-image-insert-inline (url image buffer marker)
  "Insert IMAGE after MARKER in buffer."
  (with-current-buffer buffer
    (goto-char marker)
    (let ((button (button-at marker)))
      (delete-region (overlay-start button) (overlay-end button))
      (delete-overlay button))
    (let ((button-start (point)) button-end image-start)
      (insert "[-]")
      (setq button-end (point))
      (end-of-line)
      (setq image-start (point))
      (put-image image image-start)
      (make-button button-start button-end
                   'action #'weechat-image--remove
                   'help-wecho "Remove Preview"
                   'follow-link t
                   'weechat-image-begin image-start
                   'weechat-image-end (point))))
  (message "Inserted inline %s %s %s" url buffer marker))

(defun weechat-image-view-next ()
  "Go to next image."
  (interactive)
  (search-forward "URL:" nil t))

(defun weechat-image-view-previous ()
  "Go to previous image."
  (interactive)
  (search-backward "URL:" nil t))

(defun weechat-image-view-remove-entry ()
  "Remove current entry."
  (interactive)
  (save-excursion
   (let ((beg
          (if (looking-at "^URL:")
              (point)
            (search-backward "URL:" nil t)))
         (end (progn
                (end-of-line)
                (search-forward "URL:" nil t))))
     (if end
         (setq end (- end 4))
       (setq end (point-max)))
     (let ((inhibit-read-only t))
       (remove-images beg end)
       (delete-region beg end)))))

(defun weechat-image-view-clear ()
  "Clear image view buffer."
  (interactive)
  (when (and (called-interactively-p 'interactive)
             (y-or-n-p "Clear buffer? "))
   (let ((inhibit-read-only t))
     (remove-images (point-min) (point-max))
     (erase-buffer))))

(defvar weechat-image-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" #'weechat-image-view-previous)
    (define-key map "n" #'weechat-image-view-next)
    (define-key map "c" #'weechat-image-view-clear)
    (define-key map "k" #'weechat-image-view-remove-entry)
    map)
  "Keymap for `weechat-image-view-mode'.")

(easy-menu-define weechat-image-view-mode-menu weechat-image-view-mode-map
  "WeeChat Image"
  '("WeeChatImage"
    ["Previous Image" weechat-image-view-previous t]
    ["Next Image" weechat-image-view-next t]
    ["Remove Image" weechat-image-view-remove-entry t]
    ["Clear Buffer" weechat-image-view-clear t]))

(define-derived-mode weechat-image-view-mode special-mode "WeechatImage"
  "Mode for the weechat-image viewer

\{weechat-image-view-mode-map}"
  :group 'weechat-image)

(defun weechat-image-insert-other-buffer (url image buffer marker)
  "Insert IMAGE into `weechat-image-buffer'."
  (with-current-buffer (get-buffer-create weechat-image-buffer)
    (weechat-image-view-mode)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
     (unless (bolp)
       (insert "\n"))
     (insert "URL: ")
     (insert-button url
                    'action (lambda (button)
                              (browse-url (button-get button 'weechat-image-url)))
                    'help-echo "Browse URL"
                    'follow-link t
                    'weechat-image-url url)
     (insert "\n")
     (let ((channel-name (buffer-name buffer)))
       (insert "Channel: ")
       (insert-button channel-name
                      'action (lambda (button)
                                (let ((buf (button-get button 'weechat-image-buffer))
                                      (mark (button-get button 'weechat-image-marker)))
                                  (when (buffer-live-p buf)
                                    (switch-to-buffer buf)
                                    (with-current-buffer buf
                                      (goto-char mark)))))
                      'help-echo "Goto buffer"
                      'follow-link t
                      'weechat-image-buffer buffer
                      'weechat-image-marker marker)
       (insert "\n"))
     (let (nick date)
       (with-current-buffer buffer
         (goto-char marker)
         (beginning-of-line)
         (setq nick (get-text-property (point) 'weechat-nick))
         (setq date (get-text-property (point) 'weechat-date)))
       (when date
         (insert "Date: " (format-time-string weechat-image-time-format date) "\n"))
       (when nick
         (insert "By: ")
         (insert-button nick
                        'action (lambda (button)
                                  (let ((buf (button-get button 'weechat-image-buffer))
                                        (nick (button-get button 'weechat-image-nick)))
                                    (with-current-buffer buf
                                      (weechat-nick-action nick))))
                        'help-echo "Nick Actions"
                        'follow-link t
                        'weechat-image-buffer buffer
                        'weechat-image-nick nick)
         (insert "\n")))
     (put-image image (point))
     (insert "\n")))
  (message "Added new image to %s" weechat-image-buffer)
  (switch-to-buffer weechat-image-buffer))

(defun weechat-image-resize (image what px)
  "Resize IMAGE.
WHAT should be either `:width' or `:height' and PX is the new size in pixel.
This function is a no-op if `weechat-image-use-imagemagick' is nil."
  (if weechat-image-use-imagemagick
      (or (create-image (plist-get (cdr image) :data) 'imagemagick t
                        what px)
          image)
    image))

(defun weechat-image--get-image (_status url buffer marker)
  (goto-char (point-min))
  (unless (looking-at "^HTTP/.+ 200 OK$")
    (kill-buffer)
    (error "Error while fetching image `%s'" url))
  (unless (search-forward "\n\n" nil t)
    (kill-buffer)
    (error "Error while fetching image `%s'.  Malformed http reply" url))
  (when (and weechat-image-size-limit
             (> (- (point-max) (point)) weechat-image-size-limit))
    (kill-buffer)
    (error "Image %s is too large (%s bytes)" url (- (point-max) (point))))
  (let* ((image (create-image (buffer-substring (point) (point-max))
                              (if weechat-image-use-imagemagick
                                  'imagemagick
                                nil)
                              t))
         (size (image-size image 'pixels)))
    (unless image
      (kill-buffer)
      (error "Image type not supported or not an image."))
    (when (and weechat-image-max-width
               (> (car size) weechat-image-max-width))
      (if weechat-image-resize
          (setq image (weechat-image-resize image :width weechat-image-max-width))
        (kill-buffer)
        (error "Image %s is too wide (%s px)" url (car size))))
    (when (and weechat-image-max-height
               (> (cdr size) weechat-image-max-height))
      (if weechat-image-resize
          (setq image (weechat-image-resize image :height weechat-image-max-width))
        (kill-buffer)
        (error "Image %s is too heigh (%s px)" url (cdr size))))
    (kill-buffer)
    (funcall weechat-image-display-func url image buffer marker)))

(defun weechat-image--do-preview (button)
  (let ((url (button-get button 'weechat-image-url))
        (buffer (button-get button 'weechat-image-buffer))
        (marker (button-get button 'weechat-image-marker)))
    (url-queue-retrieve url
                        #'weechat-image--get-image
                        (list url buffer marker))))

(defun weechat-image--add-preview-button ()
  "Add preview buttons after image urls."
  (goto-char (point-min))
  (search-forward "http" nil t)
  (let ((url (thing-at-point 'url)))
    (when (and url
               (s-matches? weechat-image-url-regex url)
               (not (s-matches? weechat-image-url-blacklist-regex url)))
      (end-of-thing 'url)
      (insert " ")
      (insert-button "[+]"
                     'action #'weechat-image--do-preview
                     'help-echo "Preview Image"
                     'follow-link t
                     'weechat-image-marker (point)
                     'weechat-image-buffer (current-buffer)
                     'weechat-image-url url)
      (unless (or (eolp) (looking-at "[[:space:]]"))
        (insert " ")))))

(add-hook 'weechat-insert-modify-hook #'weechat-image--add-preview-button)

(provide 'weechat-image)

;;; weechat-image.el ends here
