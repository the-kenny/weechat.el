;;; weechat --- Chat via WeeChat's relay protocol in Emacs ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Moritz Ulrich

;; Author: Moritz Ulrich (moritz@tarn-vedra.de)
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

;; This package provides a way to chat via WeeChat's relay protocol in
;; Emacs.

;; Please see README.org on how to use it.

;;; Code:

(require 'weechat-relay)
(require 'cl-lib)
(require 'rx)
(require 'format-spec)

(defgroup weechat nil
  "Weechat based IRC client for Emacs."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-"
  :group 'applications)

(defcustom weechat-read-only t
  "Whether to make text in weechat buffers read-only."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-initial-lines 100
  "Number of lines to show when initializing a channel buffer."
  :type 'integer
  :group 'weechat)

(defcustom weechat-prompt "[%n] "
  "The Weechat prompt."
  :type 'string
  :group 'weechat)

(defcustom weechat-hide-like-weechat t
  "Hide lines in buffer when they're hidden in Weechat."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-connect-hook nil
  "Hook run when successfully connected and authenticated."
  :type '(choice (const :tag "Off" nil)
                 (function :tag "Hook"))
  :group 'weechat)

(defcustom weechat-auto-reconnect-buffers t
  "Automatically re-monitor channel buffers which were opened on a prior connection."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-auto-monitor-buffers ()
  "List of buffer names to auto-monitor on connect.

If value is a list, buffers corresponding the names will be
monitored on connect. A value of t will monitor all available
buffers. Be warned, a too long list will use much bandwidth on
connect."
  :type '(choice (const :tag "All" t)
                 (repeat :tag "List" string )))

(defcustom weechat-time-format "%H:%M:%S"
  "How to format time stamps.
See `format-time-string' for format description."
  :type 'string
  :group 'weechat)

(defface weechat-time-face '((t :inherit default))
  "Weechat face used for timestamps."
  :group 'weechat)

(defface weechat-prompt-face '((t :inherit minibuffer-prompt))
  "Weechat face used for the prompt."
  :group 'weechat)

(defface weechat-highlight-face '((t :background "light blue"))
  "Weechat face for highlighted lines."
  :group 'weechat)

(defcustom weechat-text-column 22
  "Column after which text will be inserted.
If `(length (concat nick timestamp))' is longer than this value,
text-column will be increased for that line."
  :type 'integer
  :group 'weechat)

(defcustom weechat-fill-text t
  "Wether weechat should fill text paragraphs automatically."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-fill-column 'frame-width
  "Column used for filling text in buffers."
  :type '(choice
          (const :tag "Frame width" 'frame-width)
          (integer :tag "Number")
          (const :tag "Default" t)))

(defcustom weechat-notification-icon nil
  "Icon used in notifications."
  :type '(choice (const :tag "No icon" nil)
                 (file :tag "Icon file"))
  :group 'weechat)

(defcustom weechat-notification-handler
  (cond
   ((featurep 'sauron) 'weechat-sauron-handler)
   ((featurep 'notifications) 'weechat-notifications-handler))
  "Function called to display notificiations."
  :type '(choice
          (const :tag "No Notifications" nil)
          (const :tag "Sauron" 'weechat-sauron-handler)
          (const :tag "DBUS" 'weechat-notifications-handler)
          (function :tag "Custom Function"))
  :group 'weechat)

(defcustom weechat-notification-mode :monitored
  "When to notify the user.

Possible values are nil (Never), :monitored (Only monitored
buffers) and t (All buffers)."
  :type '(choice
          (const :tag "Never" nil)
          (const :tag "Monitored buffers" :monitored)
          (const :tag "All Buffers" t)))

(defcustom weechat-header-line-format "%n on %c/%s: %t"
  "Header line format.
Set to nil to disable header line.  Currently only supported format option is %t for the title."
  :type '(choice (const :tag "Disabled" nil)
                 string)
  :set (lambda (sym val)
         (set sym val)
         (when (fboundp 'weechat-update-header-line)
           (weechat-update-header-line)))
  :group 'weechat)

(defcustom weechat-input-ring-size 20
  "Size for the input ring."
  :type 'integer
  :group 'weechat)

(defvar weechat-debug-strip-formatting nil)

(defvar weechat--buffer-hashes (make-hash-table :test 'equal))

(defvar weechat--connected nil)

(defvar weechat-inhibit-notifications nil
  "Non-nil means don't display any weechat notifications.")

(defun weechat-connected-p ()
  (and (weechat-relay-connected-p)
       weechat--connected))

(defun weechat-buffer-hash (buffer-ptr)
  (gethash buffer-ptr weechat--buffer-hashes))

(defun weechat--clear-buffer-store ()
  (clrhash weechat--buffer-hashes))

(defun weechat--store-buffer-hash (ptr alist &optional replace)
  (when (and (not replace) (weechat-buffer-hash ptr))
    (error "Buffer '%s' already exists" ptr))
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (x alist)
      (puthash (car x) (cdr x) hash))
    (puthash ptr hash weechat--buffer-hashes)))

(defun weechat--remove-buffer-hash (ptr)
  (unless (weechat-buffer-hash ptr)
    (error "Buffer '%s' doesn't exist" ptr))
  (remhash ptr weechat--buffer-hashes))

(defun weechat--handle-buffer-list (response)
  ;; Remove all hashes not found in the new list
  (let* ((hdata (car response))
         (buffer-pointers (mapcar (lambda (x) (car (weechat--hdata-value-pointer-path x)))
                                  (weechat--hdata-values hdata))))
    (maphash (lambda (k _)
               (unless (cl-find k buffer-pointers
                                :test 'equal)
                 (remhash k weechat--buffer-hashes)))
             (copy-hash-table weechat--buffer-hashes))
    ;; Update all remaining values
    (dolist (value (weechat--hdata-values hdata))
      (let* ((buffer-ptr (car (weechat--hdata-value-pointer-path value)))
             (buffer-hash (weechat-buffer-hash buffer-ptr))
             (alist (weechat--hdata-value-alist value)))
        (if (hash-table-p buffer-hash)
            (dolist (v alist)
              (puthash (car v) (cdr v) buffer-hash))
          (weechat--store-buffer-hash buffer-ptr alist))))))

(defun weechat-update-buffer-list (&optional callback)
  (weechat-relay-send-command
   "hdata buffer:gui_buffers(*) number,name,short_name,title,local_variables"
   (lambda (response)
     (weechat--handle-buffer-list response)
     (when (functionp callback)
       (funcall callback)))))

(defun weechat--handle-buffer-opened (response)
  (let* ((hdata (car response))
         (value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value))))
    (when (weechat-buffer-hash buffer-ptr)
      (error "Received '_buffer_opened' event for '%s' but the buffer exists already!" buffer-ptr))
    (weechat--store-buffer-hash buffer-ptr (weechat--hdata-value-alist value))))

(defun weechat--handle-buffer-closed (response)
  (let* ((hdata (car response))
         (value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value))))
    (unless (weechat-buffer-hash buffer-ptr)
      (error "Received '_buffer_closed' event for '%s' but the buffer doesn't exist" buffer-ptr))
    (weechat--remove-buffer-hash buffer-ptr)))

(defmacro weechat->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defmacro weechat-> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defun weechat--handle-buffer-renamed (response)
  (let* ((hdata (car response))
         (value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value)))
         (hash (weechat-buffer-hash buffer-ptr)))
    (unless hash
      (error "Received '_buffer_renamed' event for '%s' but the buffer doesn't exist" buffer-ptr))
    (puthash "number" (assoc-default "number" value) hash)
    (puthash "full_name" (assoc-default "full_name" value) hash)
    (puthash "short_name" (assoc-default "short_name" value) hash)
    (puthash "local_variables" (assoc-default "local_variables" value) hash)))

(weechat-relay-add-id-callback "_buffer_opened" #'weechat--handle-buffer-opened nil 'force)
(weechat-relay-add-id-callback "_buffer_closing" #'weechat--handle-buffer-closed nil 'force)
(weechat-relay-add-id-callback "_buffer_renamed" #'weechat--handle-buffer-renamed nil 'force)

(defvar weechat-topic nil
  "Topic of the channel buffer.")

(defun weechat--handle-buffer-title-changed (response)
  (let* ((hdata (car response))
         (value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value)))
         (hash (weechat-buffer-hash buffer-ptr))
         (alist (weechat--hdata-value-alist value))
         (buffer (gethash :emacs/buffer hash))
         (new-title (or (cdr (assoc-string "title" alist)) "")))
    (unless (weechat-buffer-hash buffer-ptr)
      (error "Received '_buffer_title_changed' event for '%s' but the buffer doesn't exist" buffer-ptr))
    (puthash "title" new-title hash)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq weechat-topic new-title)
        (weechat-update-header-line-buffer buffer)))))

(weechat-relay-add-id-callback "_buffer_title_changed" #'weechat--handle-buffer-title-changed nil 'force)

(defun weechat-merge-alists (old new)
  (dolist (k new old)
    (let ((to-remove (assoc-string (car k) old)))
      (setq old (cons k (remove to-remove old))))))

(defun weechat--handle-localvar-changed (response)
  (let* ((hdata (car response))
         (value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value)))
         (hash (weechat-buffer-hash buffer-ptr))
         (alist (weechat--hdata-value-alist value))
         (buffer (gethash :emacs/buffer hash))
         (old-local-variables (gethash "local_variables" hash))
         (new-local-variables (cdr (assoc-string "local_variables" alist))))
    (unless (weechat-buffer-hash buffer-ptr)
      (error "Received '_buffer_localvar_changed' event for '%s' but the buffer doesn't exist"
             buffer-ptr))
    (puthash "local_variables"
             (weechat-merge-alists old-local-variables new-local-variables)
             hash)
    (when buffer
      (with-current-buffer buffer
        (weechat-update-prompt)
        (weechat-update-header-line-buffer buffer)))))

(weechat-relay-add-id-callback "_buffer_localvar_changed" #'weechat--handle-localvar-changed nil 'force)
(weechat-relay-add-id-callback "_buffer_localvar_added" #'weechat--handle-localvar-changed nil 'force)

;;;###autoload
(defun weechat-connect (host port password)
  (interactive (list (read-string "Relay Host: ")
                     (read-number "Port: ")
                     (read-passwd "Password: ")))
  (when (weechat-relay-connected-p)
    (if (y-or-n-p "Already connected.  Disconnect other connection? ")
        (weechat-relay-disconnect)
      (error "Can't open two connections")))

  (when (and (stringp host)
             (integerp port))
    (weechat-relay-connect
     host port
     (lambda ()
       (weechat-relay-authenticate password)
       (weechat-relay-send-command
        "info version"
        (lambda (data)
          (message "Connected to '%s', version %s" host (cdar data))
          (weechat-update-buffer-list
           (lambda ()
             (weechat-relay-send-command "sync")
             (setq weechat--connected t)
             (run-hooks 'weechat-connect-hook)))))))))

(defun weechat-disconnect ()
  (interactive)
  (weechat-relay-disconnect)
  (clrhash weechat--buffer-hashes)
  (setq weechat--connected nil))

(defun weechat-handle-disconnect ()
  (setq weechat--connected nil)
  ;; Print 'disconnected' message to all channel buffers
  (maphash (lambda (k v)
             (when (bufferp (gethash :emacs/buffer v))
               (with-current-buffer (gethash :emacs/buffer v)
                 (weechat-print-line k "!!!" "Lost connection to relay server"))))
           weechat--buffer-hashes))

(add-hook 'weechat-relay-disconnect-hook 'weechat-handle-disconnect)

(defun weechat-buffer-name (buffer-ptr)
  (let ((hash (weechat-buffer-hash buffer-ptr)))
    (or (gethash "name"        hash)
        (gethash "full_name"   hash)
        (gethash "short_name"  hash))))

(defun weechat--find-buffer (name)
  (let (ret)
    (maphash
     (lambda (ptr _)
       (let ((bname (weechat-buffer-name ptr)))
         (when (or (equal bname  name)
                   (equal bname  name)
                   (equal bname  name))
           (setq ret ptr))))
     weechat--buffer-hashes)
    ret))

(defun weechat-channel-names ()
  (let (ret)
    (maphash
     (lambda (k _)
       (setq ret (cons (weechat-buffer-name k) ret)))
     weechat--buffer-hashes)
    ret))

(defun weechat-buffer-list ()
  "List all Weechat buffers."
  (let (acc)
    (maphash (lambda (_ v)
               (when (buffer-live-p (gethash :emacs/buffer v))
                 (setq acc (cons (gethash :emacs/buffer v) acc))))
             weechat--buffer-hashes)
    acc))

(defun weechat--emacs-buffer (buffer-ptr)
  (let ((hash (gethash buffer-ptr weechat--buffer-hashes)))
    (gethash :emacs/buffer hash)))

(defvar weechat-buffer-ptr nil
  "The pointer of the channel buffer.
Used to identify it on the relay server.")
(defvar weechat-server-buffer nil
  "The relay buffer associated with this channel buffer.")
(defvar weechat-buffer-number nil)
(defvar weechat-local-prompt)

;;; Borrowed this behavior from rcirc
(defvar weechat-prompt-start-marker)
(defvar weechat-prompt-end-marker)

(defun weechat-update-prompt ()
  (save-excursion
    (let ((start (marker-position weechat-prompt-start-marker))
          (inhibit-read-only t))
      (setq weechat-local-prompt
            (format-spec weechat-prompt
                         (format-spec-make ?n (weechat-get-local-var "nick"))))
      (delete-region weechat-prompt-start-marker weechat-prompt-end-marker)
      (goto-char weechat-prompt-end-marker)
      (insert-before-markers weechat-local-prompt)
      (set-marker weechat-prompt-start-marker start)
      (unless (zerop (- weechat-prompt-end-marker
                        weechat-prompt-start-marker))
        (add-text-properties weechat-prompt-start-marker
                             weechat-prompt-end-marker
                             (list 'face 'weechat-prompt-face
                                   'read-only t
                                   'field t
                                   'rear-nonsticky t
                                   'front-sticky t))))))

(defvar weechat-formatting-regex
  (let* ((attr `(in "*!/_|"))   ;NOTE:  is not documented
         (std  `(= 2 digit))
         (astd `(seq ,attr (= 2 digit)))
         (ext  `(seq "@" (= 5 digit)))
         (aext `(seq "@" ,attr (= 5 digit))))
    (rx-form
     `(or (seq ""
               (or ,std
                   ,ext
                   (seq "F" (or ,std ,astd ,ext ,aext))
                   (seq "B" (or ,std ,ext))
                   (seq "*" (or ,std
                                ,astd
                                ,ext
                                ,aext
                                (seq (or ,std ,astd ,ext ,aext)
                                     ","
                                     (or ,std ,astd ,ext ,aext))))
                   (seq "b" (in "FDB_-#il"))
                   ""))
          (seq "" ,attr)
          (seq "" ,attr)
          ""))))

(defun weechat-strip-formatting (string)
  "Strip weechat color codes from STRING."
  (replace-regexp-in-string weechat-formatting-regex "" string))

(defcustom weechat-color-list '(unspecified "black" "dark gray" "dark red" "red"
                                            "dark green" "light green" "brown"
                                            "yellow" "dark blue" "light blue"
                                            "dark magenta" "magenta" "dark cyan"
                                            "light cyan" "gray" "white")
  "Mapping of Weechat colors.
See URL `http://www.weechat.org/files/doc/devel/weechat_dev.en.html#color_codes_in_strings'."
  :type '(repeat (choice (const :tag "Unspecified" unspecified)
                         (const :tag "Color" color)))
  :group 'weechat)

(defvar weechat-color-attributes-alist '((?* . (:weight  bold))    ; bold
                                         (?! . (:inverse-video t)) ; reverse??
                                         (?/ . (:slant  italic))   ; italic
                                         (?_ . (:underline t))     ; underline
                                         (?| . keep))              ; keep
  "Map color attribute specifiers to Emacs face property.")

(defun weechat--match-color-code (what str i)
  "Match std, ext, attr WHAT on STR at position I.
This is internal and used by `weechat-handle-color-codes'."
  (when (symbolp what)
    (cl-case what
      ((std)
       (let ((std (substring str i (+ i 2))))
         (when (s-matches? "^[0-9]+$" std)
           (list 'std (+ i 2) (string-to-number std)))))
      ((ext)
       (when (= (aref str i) ?@)
         (let ((std (substring str (1+ i) (+ i 6))))
           (when (s-matches? "^[0-9]+$" std)
             (list 'ext (+ i 6) (string-to-number std))))))
      ((attr)
       (let* ((a (aref str i))
              (x (cdr (assq a weechat-color-attributes-alist))))
         (when x
           (list 'attr (1+ i) x))))
      (t (error "Unknown parameter %s" what)))))

(defun weechat--color-keep-attributes (old-face)
  "Remove color settings from OLD-FACE but keep the attributes."
  (cl-delete-if (lambda (x)
                  (memq (car x) '(:foreground :background)))
                old-face))

(defun weechat--color-handle-F (str i old-face)
  "Handle ?F (A)STD|(A)EXT color code in STR at I with OLD-FACE.
This is an internal function of `weechat-handle-color-codes'."
  (let (match-data
        face
        (j (1+ i)))
    (while (setq match-data (weechat--match-color-code 'attr str j)) ;; (A)
      (if (eq (cl-third match-data) 'keep)
          (setq face (weechat--color-keep-attributes old-face))
        (setq face (append (list (cl-third match-data)) face)))
      (setq j (cl-second match-data)))
    (setq match-data (weechat--match-color-code 'std str j))
    (if match-data
        (setq face (append (list (list :foreground (nth (cl-third match-data)
                                                        weechat-color-list)))
                           face)) ;; TODO set attribute instead of simply append
      (setq match-data (weechat--match-color-code 'ext str j))
      (if match-data
          t ;; TODO ext
        (error "Broken color code (in ?F '%s' %s)" str i)))
    (cl-values (cl-second match-data)
               face)))

(defun weechat-handle-color-codes (str &optional i ret face)
  "Convert the Weechat color codes in STR to properties.
Currently only Fxx and Bxx are handled.  Any color codes left are stripped.

Be aware that Weechat does not use mIRC color codes.
See URL `http://www.weechat.org/files/doc/devel/weechat_dev.en.html#color_codes_in_strings'.

The optional paramteres are internal!"
  (setq i (or i 0))
  (if (>= i (length str))
      (or ret "")
    (setq ret (or ret ""))
    (cl-case (aref str i)
      ((?\x19) ;; STD|EXT|?F((A)STD|(A)EXT)|?B(STD|EXT)|?\x1C|?*...|?b...
       (let ((old-face face)
             (next (aref str (1+ i))))
         (setq face nil)
         (setq i (1+ i))
         (cond
          ((and (<= ?0 next) (<= next ?9)) ;; STD
           (let ((match-data (weechat--match-color-code 'std str i)))
             (when match-data
               ;; TODO
               (setq i (cl-second match-data)))))
          ((= next ?@) ;; EXT
           (let ((match-data (weechat--match-color-code 'ext str i)))
             (when match-data
               ;; TODO
               (setq i (cl-second match-data)))))
          ((= next ?F) ;; ?F(A)STD|?F(A)EXT
           (cl-multiple-value-setq (i face) (weechat--color-handle-F str i old-face)))
          ((= next ?B) ;; ?BSTD|?BEXT
           (let ((match-data (weechat--match-color-code 'std str i)))
             (if match-data
                 (setq face (list (list :background (nth (cl-third match-data)
                                                         weechat-color-list))))
               (setq match-data (weechat--match-color-code 'ext str i))
               (if match-data
                   t ;; TODO ext
                 (error "Broken color code (in ?B '%s' %s)" str i)))
             (when match-data
               (setq i (cl-second match-data)))))
          ((= next ?*) ;; (A)STD | (A)EXT | (A)STD ?, (A)STD | ...
           (cl-multiple-value-setq (i face) (weechat--color-handle-F str i old-face))
           (if (= (aref str i) ?,)
               (let* ((i (1+ i))
                      (match-data (weechat--match-color-code 'std str i)))
                 (if match-data
                     (setq face (append (list (list :background (nth (cl-third match-data)
                                                                     weechat-color-list)))
                                        face))
                   (setq match-data (weechat--match-color-code 'ext str i))
                   (if match-data
                       t ;; TODO ext
                     (error "Broken color code (in ?* '%s' %s)" str i)))
                 (setq i (cl-second match-data)))))
          ((= next ?b) 'b) ;; ignore for now
          ((= next ?\x1C)  ;; Clear color, leave attributes
           (setq face (weechat--color-keep-attributes old-face))))))

      ((?\x1A) ;; Set ATTR
       (let ((match-data (weechat--match-color-code 'attr str (1+ i))))
         (unless match-data
           (error "Broken color code (in ?\\x1A '%s' %s)" str i))
         (if (eq (cl-third match-data) 'keep)
             (setq face (weechat--color-keep-attributes face))
           (setq face (list (cl-third match-data))))
         (setq i (cl-second match-data))))

      ((?\x1B) ;; Delete ATTR
       (let ((match-data (weechat--match-color-code 'attr str (1+ i)))
             (old-face (copy-sequence face)))
         (unless match-data
           (error "Broken color code (in ?\\x1B '%s' %s)" str i))
         (if (eq (cl-third match-data) 'keep)
             (setq face nil) ;; TODO Does keep here means delete all or keep all?
           (setq face (delq (cl-third match-data) old-face)))
         (setq i (cl-second match-data))))

      ((?\x1C) (setq i (1+ i) face nil))) ;; reset face
    (let ((r (string-match-p "\\(\x19\\|\x1A\\|\x1B\\|\x1C\\)" str i)))
      (if r
          (weechat-handle-color-codes
           str r (concat ret
                         (propertize  (substring str i r) 'face face))
           face)
        (concat ret (propertize (substring str i) 'face face))))))

(defvar weechat--last-notification-id nil
  "Last notification id parameter for :replaces-id.")

(defun weechat-notifications-handler (sender text &optional _date _buffer-name)
  (when (and (featurep 'notifications) (fboundp 'notifications-notify))
    (setq weechat--last-notification-id
          (notifications-notify
           :title (xml-escape-string (concat "Weechat.el: Message from <"
                                             (weechat-strip-formatting sender)
                                             ">"))
           :body (xml-escape-string text)
           :app-icon weechat-notification-icon
           :replaces-id weechat--last-notification-id))))

(defun weechat-sauron-handler (sender _text &optional _date _buffer-name)
  (when (and (featurep 'sauron) (fboundp 'sauron-add-event))
    (lexical-let ((jump-position (point-max-marker)))
      (sauron-add-event 'weechat 3
                        (format "Message from %s"
                                (weechat-strip-formatting sender))
                        (lambda ()
                          (when (fboundp 'sauron-switch-to-marker-or-buffer)
                            (sauron-switch-to-marker-or-buffer jump-position)))))))


(defun weechat-notify (sender text &optional date buffer-name)
  (when (and (functionp weechat-notification-handler)
             (or (eq weechat-notification-mode t)
                 (and (eql weechat-notification-mode :monitored)
                      (local-variable-p 'weechat-buffer-ptr)
                      (buffer-live-p (weechat--emacs-buffer weechat-buffer-ptr)))))
    (funcall weechat-notification-handler sender text date buffer-name)))

(defun weechat-print-line (buffer-ptr sender text &optional date highlight)
  (setq text   (or text ""))
  (setq sender (or sender ""))
  (let ((buffer (weechat--emacs-buffer buffer-ptr)))
    (unless (bufferp buffer)
      (error "Couldn't find Emacs buffer for weechat-buffer %s" buffer-ptr))
    (with-current-buffer buffer
      (let ((at-end (= (point) weechat-prompt-end-marker))
            (old-point (point-marker)))
        (let ((inhibit-read-only t))
          (goto-char (marker-position weechat-prompt-start-marker))

          (save-restriction
            ;; Hack borrowed from rcirc:
            ;; temporarily set the marker insertion-type because
            ;; insert-before-markers results in hidden text in new buffers
            (set-marker-insertion-type weechat-prompt-start-marker t)
            (set-marker-insertion-type weechat-prompt-end-marker t)

            (narrow-to-region (point-at-bol)
                              weechat-prompt-start-marker)

            (when date
              (insert (propertize
                       (format-time-string weechat-time-format date)
                       'face 'weechat-time-face)
                      " "))

            (unless (s-blank? (weechat-handle-color-codes sender))
              (insert (weechat-handle-color-codes sender) ": "))

            (let ((chars-to-insert
                   (- weechat-text-column
                      (- (point-max) (point-min)))))
              (when (> chars-to-insert 0)
                (insert-char ?\s chars-to-insert)))

            (let ((prefix-string (make-string (- (point-max) (point-min)) ?\s))
                  (text-start (point)))

              (let ((text (weechat-handle-color-codes
                           (s-trim text))))
                (insert (if highlight
                            (propertize text 'face 'weechat-highlight-face)
                          text)
                        "\n"))

              (when weechat-fill-text
                ;; Filling is slightly misleading here. We use this
                ;; awesome text property called `wrap-prefix'.
                (let ((overlay (make-overlay text-start (point-max))))
                  (overlay-put overlay 'wrap-prefix prefix-string)))))

          (when weechat-read-only
            (add-text-properties (point-min) weechat-prompt-start-marker
                                 '(read-only t))))

        ;; Restore old position
        (let ((p-to-go (if at-end weechat-prompt-end-marker old-point))
              (w (get-buffer-window buffer)))
          ;; ...for non-active buffers (in windows)
          (when (and (not (eq (selected-window) w))
                     (eq (current-buffer)
                         (window-buffer w)))
            (set-window-point w p-to-go))

          ;; ...for active buffer
          (goto-char p-to-go))

        (set-marker-insertion-type weechat-prompt-start-marker nil)
        (set-marker-insertion-type weechat-prompt-end-marker nil))

      ;; Drop undo information (borrowed from weechat)
      (buffer-disable-undo)
	  (buffer-enable-undo))))

(defun weechat-line-type (line-hdata)
  ;; TODO: Is tags only available on 0.4.0?
  (let ((tags (mapcar (lambda (x) (intern-soft (concat ":" x)))
                      (assoc-default "tags_array" line-hdata))))
    (cond
     ((memq :irc_action tags) :irc/action)
     ((memq :irc_quit tags) :irc/quit)
     ((memq :irc_privmsg tags) :irc/privmsg)
     (:irc/privmsg)                     ;fallback
     )))

(defun weechat-print-irc-action (buffer-ptr sender message date highlight)
  (let ((weechat-text-column 0))
    (weechat-print-line buffer-ptr
                        nil
                        (concat sender message)
                        date
                        highlight)))

(defun weechat-print-line-data (line-data)
  (let* ((buffer-ptr (assoc-default "buffer" line-data))
         (buffer (weechat--emacs-buffer buffer-ptr)))
    (unless (weechat-buffer-hash buffer-ptr)
      (error "Received new line for '%s' but the buffer doesn't exist in local cache" buffer-ptr))
    (let ((sender (assoc-default "prefix" line-data))
          (message (assoc-default "message" line-data))
          (date (assoc-default "date" line-data))
          (highlight (assoc-default "highlight" line-data nil 0))
          (line-type (weechat-line-type line-data))
          (visible (= 1 (assoc-default "displayed" line-data nil 0))))
      (setq highlight (= 1 highlight))
      (when (and (bufferp (weechat--emacs-buffer buffer-ptr))
                 (and weechat-hide-like-weechat
                      visible))
        (when weechat-debug-strip-formatting
          (setq sender (weechat-strip-formatting sender))
          (setq message (weechat-strip-formatting message)))

        ;; Print the line
        (cl-case line-type
          (:irc/action
           (weechat-print-irc-action buffer-ptr
                                     nil
                                     (concat sender message)
                                     date
                                     highlight))
          (t
           (progn
             (weechat-print-line buffer-ptr
                                 sender
                                 message
                                 date
                                 highlight)))))

      ;; TODO: Debug highlight for monitored and un-monitored channels
      ;; (Maybe) notify the user
      (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                               weechat-relay-log-buffer-name)
        (when (and (not weechat-inhibit-notifications) highlight)
          (weechat-notify sender message date (buffer-name)))))))

(defun weechat-add-initial-lines (response)
  (let* ((lines-hdata (car response))
         (buf-ptr (weechat->
                   lines-hdata
                   (weechat--hdata-values)
                   (car)
                   (weechat--hdata-value-pointer-path)
                   (car))))
    ;; Need to get buffer-ptr from hdata pointer list
    (with-current-buffer (weechat--emacs-buffer buf-ptr)
      (save-excursion
        (let ((weechat-inhibit-notifications t))
          (dolist (line-hdata (weechat--hdata-values lines-hdata))
            (weechat-print-line-data (weechat--hdata-value-alist line-hdata))))))))

(defvar weechat-initial-lines-buffer-properties
  '("message" "highlight" "prefix" "date" "buffer" "displayed" "tags_array"))

(defun weechat-request-initial-lines (buffer-ptr)
  (weechat-relay-send-command
   (format "hdata buffer:%s/lines/last_line(-%i)/data %s"
           buffer-ptr
           weechat-initial-lines
           (s-join "," weechat-initial-lines-buffer-properties))
   #'weechat-add-initial-lines))

(defun weechat-send-input (target input)
  (weechat-relay-send-command
   (format "input %s %s" target input)))

(defun weechat-get-input ()
  (s-trim-right
   (buffer-substring-no-properties
    weechat-prompt-end-marker
    (point-max))))

(defun weechat-replace-input (replacement)
  (save-excursion
    (delete-region weechat-prompt-end-marker (point-max))
    (goto-char weechat-prompt-end-marker)
    (insert (or replacement ""))))

(defvar weechat-input-ring)

(defun weechat-input-ring-insert (input)
  (unless (ring-member weechat-input-ring input)
    (ring-insert weechat-input-ring input)))

(defun weechat-previous-input ()
  (interactive)
  (let ((input (weechat-get-input)))
    ;; If input isn't in the ring, assume push it in and show first
    (cond
     ((string= input "")
      (weechat-replace-input (ring-ref weechat-input-ring 0)))
     ((not (ring-member weechat-input-ring input))
      (weechat-replace-input (ring-ref weechat-input-ring 0))
      (weechat-input-ring-insert input))
     ((ring-member weechat-input-ring input)
      (if (string= (ring-ref weechat-input-ring
                             (1- (ring-length weechat-input-ring)))
                   input)
          (weechat-replace-input "")
        (weechat-replace-input (ring-next weechat-input-ring input)))))))

(defun weechat-next-input ()
  (interactive)
  (let ((input (weechat-get-input)))
    ;; If input isn't in the ring, assume push it in and show first
    (cond
     ((string= input "")
      (weechat-replace-input
       (ring-ref weechat-input-ring
                 (1- (ring-length weechat-input-ring)))))
     ((not (ring-member weechat-input-ring input))
      (weechat-input-ring-insert input)
      (weechat-replace-input (1- (ring-length weechat-input-ring))))
     ((ring-member weechat-input-ring input)
      (if (string= (ring-ref weechat-input-ring 0)
                   input)
          (weechat-replace-input "")
        (weechat-replace-input (ring-previous weechat-input-ring input)))))))

(defun weechat-return ()
  (interactive)
  (cond
   ((> (point) weechat-prompt-end-marker)
    ;; Submit
    (let ((input (weechat-get-input)))
      (unless (string= "" input)
        (dolist (l (split-string input "\n"))
          (weechat-send-input weechat-buffer-ptr l))
        (weechat-input-ring-insert input)
        (weechat-replace-input ""))))
   ((< (point) weechat-prompt-start-marker)
    ;; Copy current line to input line
    (weechat-replace-input
     (buffer-substring-no-properties
      (point-at-bol) (point-at-eol)))
    (goto-char (point-max)))))

(defvar weechat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'weechat-return)
    (define-key map (kbd "M-p") 'weechat-previous-input)
    (define-key map (kbd "M-n") 'weechat-next-input)
    (define-key map (kbd "C-c C-r") 'weechat-reload-buffer)
    map)
  "Keymap for weechat mode.")

(easy-menu-define weechat-mode-menu weechat-mode-map
  "Weechat menu"
  '("WeeChat"
    ["Previous Input" weechat-previous-input t]
    ["Next Input" weechat-next-input t]
    "-"
    ["Reload Buffer" weechat-reload-buffer t]
    ["Close Buffer" kill-buffer t]
    ["Monitor Buffer" weechat-monitor-buffer t]
    ["Disconnect" weechat-disconnect t]))

(defun weechat-get-local-var (var &optional buffer-ptr)
  "Return value of local VAR in BUFFER-PTR.
Default is current buffer."
  (or (cdr (assoc-string var
                         (gethash "local_variables"
                                  (weechat-buffer-hash
                                   (or buffer-ptr weechat-buffer-ptr)))))
      ""))

(defun weechat-update-header-line-buffer (buffer)
  "Update the header line for BUFFER."
  (with-current-buffer buffer
    (let ((spec (format-spec-make
                 ?n (weechat-get-local-var "nick")
                 ?s (weechat-get-local-var "server")
                 ?c (weechat-get-local-var "channel")
                 ?N (weechat-get-local-var "name")
                 ?t weechat-topic)))
      (if weechat-header-line-format
          (setq header-line-format (format-spec weechat-header-line-format spec))
        (setq header-line-format nil)))))

(defun weechat-update-header-line (&optional buffer)
  "Update the header line for BUFFER or if BUFFER is nil for all buffers."
  (if (and buffer (bufferp buffer))
      (weechat-update-header-line-buffer buffer)
    (dolist (buffer (weechat-buffer-list))
      (weechat-update-header-line-buffer buffer))))

(defun weechat-mode (process buffer-ptr buffer-hash)
  "Major mode used by weechat buffers.

\\{weechat-mode-map}"

  (kill-all-local-variables)

  (puthash :emacs/buffer (current-buffer) buffer-hash)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (hash-table-p weechat--buffer-hashes)
                (let ((hash (weechat-buffer-hash weechat-buffer-ptr)))
                  (when (hash-table-p hash)
                    (remhash :emacs/buffer hash)))))
            nil
            'local-hook)

  (use-local-map weechat-mode-map)
  (setq mode-name (format "weechat: %s" (weechat-buffer-name buffer-ptr)))
  (setq major-mode 'weechat-mode)

  (set (make-local-variable 'weechat-buffer-ptr) buffer-ptr)
  (set (make-local-variable 'weechat-server-buffer) (process-buffer process))
  (set (make-local-variable 'weechat-buffer-number) (gethash "number" buffer-hash))
  (set (make-local-variable 'weechat-topic) (gethash "title" buffer-hash))

  (make-local-variable 'weechat-local-prompt)
  (set (make-local-variable 'weechat-prompt-start-marker) (point-max-marker))
  (set (make-local-variable 'weechat-prompt-end-marker) (point-max-marker))
  (weechat-update-prompt)

  (set (make-local-variable 'weechat-input-ring) (make-ring weechat-input-ring-size))

  ;; Don't auto-add newlines on next-line
  (set (make-local-variable 'next-line-add-newlines) nil)

  ;; Initialize buffer
  (weechat-request-initial-lines buffer-ptr)

  ;; Set Header
  (weechat-update-header-line-buffer (current-buffer))

  ;; Hooks
  (run-mode-hooks 'weechat-mode-hook))


(defun weechat-monitor-buffer (buffer-ptr &optional show-buffer)
  (interactive (list
                (weechat--find-buffer
                 (funcall (or (and (featurep 'ido)
                                   (symbol-function 'ido-completing-read))
                              #'completing-read)
                          "Channel Name: " (weechat-channel-names)))
                t))
  (save-excursion
    (let* ((buffer-hash (weechat-buffer-hash buffer-ptr))
           (name (weechat-buffer-name buffer-ptr)))
      (unless (hash-table-p buffer-hash)
        (error "Couldn't find buffer %s on relay server" buffer-ptr))

      (with-current-buffer (get-buffer-create name)
        (fundamental-mode)
        (let ((inhibit-read-only t))
          (kill-region (point-min) (point-max)))
        (weechat-mode (get-buffer-process weechat-relay-buffer-name)
                      buffer-ptr
                      buffer-hash)
        (when show-buffer
          (switch-to-buffer (current-buffer)))))))

(defun weechat-reload-buffer (&optional buffer)
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (weechat-relay-log
     (format "Re-monitoring buffer %S" buffer))
    (weechat-monitor-buffer weechat-buffer-ptr)))

(defun weechat-re-monitor-buffers ()
  (when weechat-auto-reconnect-buffers
    (maphash (lambda (_ hash)
               (let ((buffer (and (gethash :emacs/buffer hash)
                                  (get-buffer (gethash :emacs/buffer hash)))))
                 (when (buffer-live-p buffer)
                   (weechat-relay-log (format "Re-monitoring buffer %S" buffer) :info)
                   (weechat-reload-buffer buffer))))
             weechat--buffer-hashes)))

(add-hook 'weechat-connect-hook 'weechat-re-monitor-buffers)

(defun weechat-auto-monitor ()
  (let ((available-channels (weechat-channel-names)))
    ;; Either iterate ALL available channels (for `t') or iterate
    ;; channels user wants to monitor
    (dolist (channel (if (listp weechat-auto-monitor-buffers)
                         weechat-auto-monitor-buffers
                       (progn
                         (message "Monitoring all available WeeChat buffers. Be patient...")
                         available-channels)))
      ;; Check if one of the available channels partially matches the
      ;; channel we want to monitor
      (let* ((channel-name (cl-some
                            (lambda (ac)
                              (when (s-contains? channel ac) ac))
                            available-channels))
             (buffer-ptr (weechat--find-buffer channel-name)))
        ;; Only auto-connect if it there isn't already a buffer monitoring the channel
        (unless (weechat--emacs-buffer buffer-ptr)
          (weechat-relay-log (format "Auto-monitoring buffer %S" channel-name) :info)
          (weechat-monitor-buffer buffer-ptr nil))))))

(add-hook 'weechat-connect-hook 'weechat-auto-monitor 'append)

(defun weechat--handle-buffer-line-added (response)
  (let* ((hdata (car response))
         (line-data (weechat--hdata-value-alist (car (weechat--hdata-values hdata)))))
    (weechat-print-line-data line-data)))

(weechat-relay-add-id-callback "_buffer_line_added" #'weechat--handle-buffer-line-added nil 'force)

(provide 'weechat)

;;; weechat.el ends here
