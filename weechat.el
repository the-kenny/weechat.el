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

;;; Code:

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
      (should (eq (cadr data)
                  (gethash "name"(weechat-buffer-hash "0xffffff")))))
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
          (weechat-update-buffer-list)))))))

(defun weechat-disconnect ()
  (interactive)
  (weechat-relay-disconnect)
  (setq weechat--buffer-alist nil))

(add-hook 'weechat-relay-disconnect-hook (lambda () (message "Disconnected from Weechat")))

(defun weechat--find-buffer (name)
  (let (ret)
   (maphash
    (lambda (ptr hash)
      (when (equal (gethash "name" hash) name)
        (setq ret ptr)))
    weechat--buffer-hashes)
   ret))

(defun weechat-channel-names ()
  (let (ret)
   (maphash (lambda (k v)
              (setq ret (cons (gethash "name" v) ret)))
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

(defun weechat-mode (process buffer-ptr buffer-hash)
  "Major mode used by weechat buffers."

  (setq mode-name "weeeechat")
  (setq major-mode 'weechat-mode)

  (set (make-local-variable 'weechat-buffer-ptr) buffer-ptr)
  (set (make-local-variable 'weechat-server-buffer) (process-buffer process))
  (set (make-local-variable 'weechat-buffer-number) (gethash "number" buffer-hash))
  (set (make-local-variable 'weechat-topic) (gethash "title" buffer-hash))

  (set (make-local-variable 'weechat-prompt-start-marker) (point-max-marker))
  (set (make-local-variable 'weechat-prompt-end-marker) (point-max-marker))

  (puthash :emacs/buffer (current-buffer) buffer-hash)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (remhash :emacs/buffer (weechat-buffer-hash weechat-buffer-ptr)))
            nil
            'local-hook))

(defun weechat-print-line (buffer-ptr sender text)
  (setq text (or text ""))
  (let ((buffer (weechat--emacs-buffer buffer-ptr)))
    (assert (bufferp buffer))
    (with-current-buffer buffer
      (let ((at-end (= (point) weechat-prompt-end-marker))
            (old-point (point-marker)))
        (let ((inhibit-read-only t))
          (when weechat-read-only
            (add-text-properties (point-min) weechat-prompt-start-marker
                                 '(read-only t)))
          (goto-char (marker-position weechat-prompt-start-marker))

          ;; Hack burrowed from rcirc:
          ;; temporarily set the marker insertion-type because
          ;; insert-before-markers results in hidden text in new buffers
          (set-marker-insertion-type weechat-prompt-start-marker t)
          (set-marker-insertion-type weechat-prompt-end-marker t)

          (insert sender "> " (s-trim text) "\n"))

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

(provide 'weechat)

;;; weechat.el ends here
