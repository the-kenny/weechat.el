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
(require 'ert)

(defvar weechat-read-only t
  "Whether to make text in weechat buffers read-only.")

(defvar weechat-initial-lines 100
  "Number of lines to show when initializing a channel buffer.")

(defvar weechat-prompt "> ")

(defvar weechat-hide-like-weechat t
  "Hide lines in buffer when they're hidden in weechat.")

;;; Code:

(defvar weechat-debug-strip-formatting t)

(defvar weechat--buffer-hashes (make-hash-table :test 'equal))

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

(defun weechat--handle-buffer-list (hdata)
  (weechat--clear-buffer-store)
  (dolist (value (weechat--hdata-values hdata))
    (let ((buffer-ptr (car (weechat--hdata-value-pointer-path value))))
      (weechat--store-buffer-hash
       buffer-ptr
       (weechat--hdata-value-alist value)))))

(defun weechat-update-buffer-list ()
  (weechat-relay-send-command
   "hdata buffer:gui_buffers(*) number,name,short_name,title,local_variables"
   #'weechat--handle-buffer-list))

(defun weechat--handle-buffer-opened (hdata)
  (let* ((value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value))))
    (when (weechat-buffer-hash buffer-ptr)
      (error "Received '_buffer_opened' event for '%s' but the buffer exists already!" buffer-ptr))
    (weechat--store-buffer-hash buffer-ptr (weechat--hdata-value-alist value))))

(defun weechat--handle-buffer-closed (hdata)
  (let* ((value (car (weechat--hdata-values hdata)))
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

(defun weechat--handle-buffer-renamed (hdata)
  (let* ((value (car (weechat--hdata-values hdata)))
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
  (when (and (stringp host)
             (integerp port))
    (weechat-relay-connect
     host port
     (lambda ()
       (weechat-relay-authenticate password)
       (weechat-relay-send-command
        "info version"
        (lambda (data)
          (message "Connected to '%s', version %s" host (cdr data))
          (weechat-update-buffer-list)
          (weechat-relay-send-command "sync")))))))

(defun weechat-disconnect ()
  (interactive)
  (weechat-relay-disconnect)
  (setq weechat--buffer-alist nil))

(add-hook 'weechat-relay-disconnect-hook (lambda () (message "Disconnected from Weechat")))

(defun weechat--find-buffer (name)
  (let (ret)
    (maphash
     (lambda (ptr hash)
       (when (or (equal (gethash "name"        hash)  name)
                 (equal (gethash "full_name"   hash)  name)
                 (equal (gethash "short_name"  hash)  name))
         (setq ret ptr)))
     weechat--buffer-hashes)
    ret))

(defun weechat-channel-names ()
  (let (ret)
    (maphash (lambda (k v)
               (setq ret (cons (or (gethash "name"        v)
                                   (gethash "full_name"   v)
                                   (gethash "short_name"  v))
                               ret)))
             weechat--buffer-hashes)
    ret))

(defun weechat--emacs-buffer (buffer-ptr)
  (let ((hash (gethash buffer-ptr weechat--buffer-hashes)))
    (assert (hash-table-p hash))
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
  (let* ((attr `(in "*!/_|"))
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
          (when weechat-read-only
            (add-text-properties (point-min) weechat-prompt-start-marker
                                 '(read-only t)))
          (goto-char (marker-position weechat-prompt-start-marker))

          ;; Hack borrowed from rcirc:
          ;; temporarily set the marker insertion-type because
          ;; insert-before-markers results in hidden text in new buffers
          (set-marker-insertion-type weechat-prompt-start-marker t)
          (set-marker-insertion-type weechat-prompt-end-marker t)

          (insert sender ": " (s-trim text) "\n"))

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
        (set-marker-insertion-type weechat-prompt-end-marker nil)))))

(defun weechat-add-initial-lines (lines-hdata)
  ;; Need to get buffer-ptr from hdata pointer list
  (let ((buffer (weechat->
                 lines-hdata
                 (weechat--hdata-values)
                 (car)
                 (weechat--hdata-value-pointer-path)
                 (car)
                 (weechat--emacs-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (kill-region (point-min) weechat-prompt-start-marker)
        (dolist (line-hdata (weechat--hdata-values lines-hdata))
          (let ((alist (weechat--hdata-value-alist line-hdata)))
            (weechat-print-line weechat-buffer-ptr
                                (assoc-default "prefix"  alist)
                                (assoc-default "message" alist))))))))

(defun weechat-request-initial-lines (buffer-ptr)
  (let ((buffer (weechat--emacs-buffer buffer-ptr)))
    (assert (bufferp buffer))
    (weechat-relay-send-command
     (format "hdata buffer:%s/lines/last_line(-%i)/data message,highlight,prefix,date"
             buffer-ptr
             weechat-initial-lines)
     #'weechat-add-initial-lines)))

(defun weechat-mode (process buffer-ptr buffer-hash)
  "Major mode used by weechat buffers."

  (kill-all-local-variables)

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
              (remhash :emacs/buffer (weechat-buffer-hash weechat-buffer-ptr)))
            nil
            'local-hook)

  ;; Initialize buffer
  (weechat-request-initial-lines buffer-ptr))

(defun weechat-monitor-buffer (name)
  (interactive (list
                (funcall (or (symbol-function 'ido-completing-read)
                             #'completing-read)
                         "Channel Name: " (weechat-channel-names))))
  (let* ((buffer-ptr (weechat--find-buffer name))
         (buffer-hash (weechat-buffer-hash buffer-ptr)))
    (when (not (hash-table-p buffer-hash))
      (error "Couldn't find buffer %s on relay server." name))

    (when (and (bufferp (get-buffer name))
               (y-or-n-p "Buffer already monitored. Replace? "))
      (kill-buffer name))

    (with-current-buffer (get-buffer-create name)
      (weechat-mode (get-buffer-process weechat-relay-buffer-name)
                    buffer-ptr
                    buffer-hash)
      (switch-to-buffer (current-buffer)))))

(defun weechat--handle-buffer-line-added (hdata)
  (let* ((value (car (weechat--hdata-values hdata)))
         (buffer-ptr (assoc-default "buffer" value)))
    (when (not (weechat-buffer-hash buffer-ptr))
      (error "Received new line for '%s' but the buffer doesn't exist in local cache" buffer-ptr))
    (when (and (bufferp (weechat--emacs-buffer buffer-ptr))
               (and weechat-hide-like-weechat
                    (= 1 (assoc-default "displayed" value))))
      (weechat-print-line buffer-ptr
                          (funcall (if weechat-debug-strip-formatting #'weechat-strip-formatting identity)
                                   (assoc-default "prefix" value))
                          (funcall (if weechat-debug-strip-formatting #'weechat-strip-formatting identity)
                                   (assoc-default "message" value))))))

(weechat-relay-add-id-callback "_buffer_line_added" #'weechat--handle-buffer-line-added nil 'force)

(provide 'weechat)

;;; weechat.el ends here
