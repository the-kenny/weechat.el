;;; weechat --- Chat via Weechat's relay protocol in Emacs ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Moritz Ulrich

;; Author: Moritz Ulrich (moritz@tarn-vedra.de)
;; Keywords: irc chat network weechat

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

(require 'weechat-relay)
(require 'cl-lib)
(require 'ert)
(require 'rx)

(defvar weechat-read-only t
  "Whether to make text in weechat buffers read-only.")

(defvar weechat-initial-lines 100
  "Number of lines to show when initializing a channel buffer.")

(defvar weechat-prompt "> ")

(defvar weechat-hide-like-weechat t
  "Hide lines in buffer when they're hidden in weechat.")

(defvar weechat-connect-hook nil
  "Hook run when successfully connected and authenticated.")

(defvar weechat-auto-reconnect-buffers t
  "Automatically re-monitor channel buffers which were opened on
  a prior connection")

;;; Code:

(defvar weechat-debug-strip-formatting t)

(defvar weechat--buffer-hashes (make-hash-table :test 'equal))

(defvar weechat--connected nil)

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
  (when (not (weechat-buffer-hash ptr))
    (error "Buffer '%s' doesn't exist" ptr))
  (remhash ptr weechat--buffer-hashes))

(ert-deftest weechat-test-buffer-store ()
  (let ((weechat--buffer-hashes (copy-hash-table weechat--buffer-hashes)))
    (weechat--clear-buffer-store)
    (should (eql 0 (hash-table-count weechat--buffer-hashes)))
    (let ((data '(("name" . "Foobar"))))
      (weechat--store-buffer-hash "0xffffff" data)
      (should (eq (cdar data)
                  (gethash "name" (weechat-buffer-hash "0xffffff")))))
    (weechat--remove-buffer-hash "0xffffff")
    (should (not (weechat-buffer-hash "0xffffff")))))

(defun weechat--handle-buffer-list (response)
  ;; Remove all hashes not found in the new list
  (let* ((hdata (car response))
         (buffer-pointers (mapcar (lambda (x) (car (weechat--hdata-value-pointer-path x)))
                                  (weechat--hdata-values hdata))))
    (maphash (lambda (k v)
               (when (not (cl-find k buffer-pointers
                                   :test 'equal))
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
          (weechat--store-buffer-hash buffer-ptr alist))))
    (run-hooks 'weechat-connect-hook)))

(defun weechat-update-buffer-list ()
  (weechat-relay-send-command
   "hdata buffer:gui_buffers(*) number,name,short_name,title,local_variables"
   #'weechat--handle-buffer-list))

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
    (when (not (weechat-buffer-hash buffer-ptr))
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
         (hash (weechat-buffer-hash buffer-ptr))
         (alist (weechat--hdata-value-alist value)))
    (when (not hash)
      (error "Received '_buffer_renamed' event for '%s' but the buffer doesn't exist" buffer-ptr))
    (puthash "number" (assoc-default "number" value) hash)
    (puthash "full_name" (assoc-default "full_name" value) hash)
    (puthash "short_name" (assoc-default "short_name" value) hash)
    (puthash "local_variables" (assoc-default "local_variables" value) hash)))

(weechat-relay-add-id-callback "_buffer_opened" #'weechat--handle-buffer-opened nil 'force)
(weechat-relay-add-id-callback "_buffer_closing" #'weechat--handle-buffer-closed nil 'force)
(weechat-relay-add-id-callback "_buffer_renamed" #'weechat--handle-buffer-renamed nil 'force)

(defun weechat-connect (host port password)
  (interactive (list (read-string "Relay Host: ")
                     (read-number "Port: ")
                     (read-passwd "Password: ")))
  (when (weechat-relay-connected-p)
    (if (y-or-n-p "Already connected. Disconnect other connection? ")
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
          (weechat-update-buffer-list)
          (weechat-relay-send-command
           "sync"
           (lambda (_) (setq weechat--connected t)))))))))

(defun weechat-disconnect ()
  (interactive)
  (weechat-relay-disconnect)
  (setq weechat--buffer-alist nil))

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
     (lambda (ptr hash)
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
     (lambda (k v)
       (setq ret (cons (weechat-buffer-name k) ret)))
     weechat--buffer-hashes)
    ret))

(defun weechat--emacs-buffer (buffer-ptr)
  (let ((hash (gethash buffer-ptr weechat--buffer-hashes)))
    (gethash :emacs/buffer hash)))

(defvar weechat-buffer-ptr nil
  "The pointer of the channel buffer. Used to identify it on the
  relay server.")
(defvar weechat-server-buffer nil
  "The relay buffer associated with this channel buffer.")
(defvar weechat-topic nil
  "Topic of the channel buffer.")
(defvar weechat-buffer-number nil)

;;; Borrowed this behavior from rcirc
(defvar weechat-prompt-start-marker)
(defvar weechat-prompt-end-marker)

(defun weechat-update-prompt ()
  (save-excursion
    (let ((start (point))
          (inhibit-read-only t))
      (delete-region weechat-prompt-start-marker weechat-prompt-end-marker)
      (insert-before-markers weechat-prompt)
      (set-marker weechat-prompt-start-marker start)
      (when (not (zerop (- weechat-prompt-end-marker
                           weechat-prompt-start-marker)))
        (add-text-properties weechat-prompt-start-marker
                             weechat-prompt-end-marker
                             (list 'read-only t
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
  "Strips weechat color codes from STRING"
  (replace-regexp-in-string weechat-formatting-regex "" string))

(ert-deftest weechat-color-stripping ()
  (should (equal (weechat-strip-formatting
                  "F14someone282728F05 has joined 13#asdfasdfasdfF05")
                 "someone has joined #asdfasdfasdf"))
  (should (equal (weechat-strip-formatting "ddd") "ddd")))


(defun weechat-print-line (buffer-ptr sender text)
  (setq text   (or text ""))
  (setq sender (or sender ""))
  (let ((buffer (weechat--emacs-buffer buffer-ptr)))
    (when (not (bufferp buffer))
      (error "Couldn't find emacs buffer for weechat-buffer %s" buffer-ptr))
    (with-current-buffer buffer
      (let ((at-end (= (point) weechat-prompt-end-marker))
            (old-point (point-marker)))
        (let ((inhibit-read-only t))
          (goto-char (marker-position weechat-prompt-start-marker))

          ;; Hack borrowed from rcirc:
          ;; temporarily set the marker insertion-type because
          ;; insert-before-markers results in hidden text in new buffers
          (set-marker-insertion-type weechat-prompt-start-marker t)
          (set-marker-insertion-type weechat-prompt-end-marker t)

          (insert sender ": ")
          (insert (s-trim text) "\n")
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

(defun weechat-print-line-data (line-data)
  (let* ((buffer-ptr (assoc-default "buffer" line-data)))
    (when (not (weechat-buffer-hash buffer-ptr))
      (error "Received new line for '%s' but the buffer doesn't exist in local cache" buffer-ptr))
    (when (and (bufferp (weechat--emacs-buffer buffer-ptr))
               (and weechat-hide-like-weechat
                    (equal 1 (assoc-default "displayed" line-data))))
      (let ((sender (assoc-default "prefix" line-data))
            (message (assoc-default "message" line-data)))
        (when weechat-debug-strip-formatting
          (setq sender (weechat-strip-formatting sender))
          (setq message (weechat-strip-formatting message)))
        (weechat-print-line buffer-ptr sender message)))))

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
        (dolist (line-hdata (weechat--hdata-values lines-hdata))
          (weechat-print-line-data (weechat--hdata-value-alist line-hdata)))))))

(defun weechat-request-initial-lines (buffer-ptr)
  (let ((buffer (weechat--emacs-buffer buffer-ptr)))
    (weechat-relay-send-command
     (format "hdata buffer:%s/lines/last_line(-%i)/data message,highlight,prefix,date,buffer,displayed"
             buffer-ptr
             weechat-initial-lines)
     #'weechat-add-initial-lines)))

(defun weechat-send-input (target input)
  (weechat-relay-send-command
   (format "input %s %s" target input)))

(defun weechat-return ()
  (interactive)
  ;; TODO: Copy current line when not in input area
  (when (and (> (point) weechat-prompt-end-marker))
    (let ((input (buffer-substring-no-properties weechat-prompt-end-marker (point-max))))
      (when (not (equal "" (s-trim input)))
        (setq input (s-trim-right input))
        (dolist (l (split-string input "\n"))
          (weechat-send-input weechat-buffer-ptr l))
        (delete-region weechat-prompt-end-marker (point-max))))))

(defvar weechat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'weechat-return)
    map)
  "Keymap for weechat mode.")

(defun weechat-mode (process buffer-ptr buffer-hash)
  "Major mode used by weechat buffers."

  (kill-all-local-variables)

  (use-local-map weechat-mode-map)
  (setq mode-name "weeeechat")
  (setq major-mode 'weechat-mode)

  (set (make-local-variable 'weechat-buffer-ptr) buffer-ptr)
  (set (make-local-variable 'weechat-server-buffer) (process-buffer process))
  (set (make-local-variable 'weechat-buffer-number) (gethash "number" buffer-hash))
  (set (make-local-variable 'weechat-topic) (gethash "title" buffer-hash))

  (set (make-local-variable 'weechat-prompt-start-marker) (point-max-marker))
  (set (make-local-variable 'weechat-prompt-end-marker) (point-max-marker))
  (weechat-update-prompt)

  (puthash :emacs/buffer (current-buffer) buffer-hash)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (let ((hash (weechat-buffer-hash weechat-buffer-ptr)))
               (when (hash-table-p hash)
                   (remhash :emacs/buffer hash))))
            nil
            'local-hook)

  ;; Initialize buffer
  (weechat-request-initial-lines buffer-ptr))

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
      (when (not (hash-table-p buffer-hash))
        (error "Couldn't find buffer %s on relay server." buffer-ptr))

      (with-current-buffer (get-buffer-create name)
        (fundamental-mode)
        (let ((inhibit-read-only t))
          (kill-region (point-min) (point-max)))
        (weechat-mode (get-buffer-process weechat-relay-buffer-name)
                      buffer-ptr
                      buffer-hash)
        (when show-buffer
          (switch-to-buffer (current-buffer)))))))

(defun weechat-re-monitor-buffers ()
  (when weechat-auto-reconnect-buffers
    (maphash (lambda (buffer-ptr hash)
               (when (and (gethash :emacs/buffer hash)
                          (buffer-live-p (get-buffer (gethash :emacs/buffer hash))))
                 (weechat-relay-log
                  (format "Re-monitoring buffer %S" (gethash :emacs/buffer hash)))
                 (weechat-monitor-buffer buffer-ptr)))
             weechat--buffer-hashes)))

(add-hook 'weechat-connect-hook 'weechat-re-monitor-buffers)

(defun weechat--handle-buffer-line-added (response)
  (let* ((hdata (car response))
         (line-data (weechat--hdata-value-alist (car (weechat--hdata-values hdata))))
         (buffer-ptr (assoc-default "buffer" line-data)))
    (weechat-print-line-data line-data)))

(weechat-relay-add-id-callback "_buffer_line_added" #'weechat--handle-buffer-line-added nil 'force)

(provide 'weechat)

;;; weechat.el ends here
