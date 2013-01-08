;; -*- lexical-binding: t -*- 
;;; weechat-relay --- Implementation of Weechat's relay protocol

;; Copyright (C) 2012 Moritz Ulrich

;; Author: Moritz Ulrich (moritz@tarn-vedra.de)
;; Version: 0.1
;; Created 30. Dec 2012
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

(require 'bindat)
(require 'ert)
(require 's)
(require 'pp)

(defvar weechat-relay-buffer-name "*weechat-relay*"
  "Buffer holding the connection to the host weechat instance.")

(defvar weechat-relay-log-buffer-name "*weechat-relay-log*"
  "Buffer name to use as debug log.
Set to nil to disable logging.")

(defvar weechat-relay-message-function nil
  "Function to call when receiving a new weechat message.")

(defvar weechat-relay-ignored-message-ids '("_nicklist")
  "IDs to ignore.")

(defvar weechat-relay-id-callback-alist '()
  "Alist mapping from ids to functions. Incoming message-ids will
be searched in this alist and the corresponding function will be
called.")


;;; Code:

(defun weechat--relay-log (text)
  "Log `TEXT' to `weechat-relay-log-buffer-name' if enabled."
  (when (bufferp (get-buffer weechat-relay-log-buffer-name))
    (save-excursion
      (with-current-buffer weechat-relay-log-buffer-name 
        (goto-char (point-max))
        (insert (s-trim text))
        (newline)))))

(defun weechat--relay-send-message (text &optional id)
  "Send message `TEXT' with optional ID `id'.
Trims `text' prior sending it."
  (let ((msg (concat (when id (format "(%s) " id)) (s-trim text) "\n")))
    (weechat--relay-log (format "Sending msg: '%s'" (s-trim msg)))
    (send-string (get-buffer-process weechat-relay-buffer-name)
                 msg)))

(defun weechat-relay-authenticate (password)
  "Authenticates to weechat with PASSWORD `password'."
  (weechat--relay-send-message (format "init password=%s,compression=off\n" password)))

(defun weechat--relay-bindat-unsigned-to-signed (num bytes)
  "Convert an unsigned int `NUM' to signed int.
`num' is in two-complement representation with `BYTES' bytes.
Useful because bindat doesn't support signed numbers."
  (if (> num (- (expt 2 (- (* 8 bytes) 1)) 1))
      (- num (expt 2 (* 8 bytes)))
    num))

(defun weechat--relay-unpack-int (data)
  "Unpack a four-byte signed integer from unibyte string `DATA'.
Returns the value and number of bytes consumed."
  (values
   (weechat--relay-bindat-unsigned-to-signed
    (bindat-get-field
     (bindat-unpack '((val u32)) data)
     'val)
    4)
   4))

(defun weechat--relay-unpack-chr (data)
  "Unpack a one byte char from unibyte string `DATA'.
Returns value and bytes consumed."
  (values
   (bindat-get-field
    (bindat-unpack '((val u8)) data)
    'val)
   1))

(defconst weechat--relay-str-spec
  '((len u32)
    (val str (eval (let ((len (weechat--relay-bindat-unsigned-to-signed
                               (bindat-get-field struct 'len)
                               4)))
                     ;; Hack for signed/unsigned problems
                     (if (<= len 0) 0 len))))))

(defun weechat--relay-unpack-str (data)
  "Unpacks a weechat-relay-string from unibyte string `DATA'.
Optional second return value contains length of parsed data."
  (let ((obj (bindat-unpack weechat--relay-str-spec data)))
    (values (decode-coding-string (bindat-get-field obj 'val) 'utf-8)
            (bindat-length weechat--relay-str-spec obj))))

(defconst weechat--relay-ptr-spec
  '((len u8)
    (val str (eval (let ((len (weechat--relay-bindat-unsigned-to-signed
                               (bindat-get-field struct 'len)
                               1)))
                     ;; Hack for signed/unsigned problems
                     (if (<= len 0) 0 len))))))

(defun weechat--relay-unpack-ptr (data)
  "Unpack a string encoded in weechat's binary representation.
`DATA' must be an unibyte string. Returns string-value and number
of bytes consumed."
  (let ((obj (bindat-unpack weechat--relay-ptr-spec data)))
    (values (concat "0x" (bindat-get-field obj 'val))
            (bindat-length weechat--relay-ptr-spec obj))))

(defconst weechat--relay-tim-spec
  '((len u8)
    (val str (eval (let ((len (weechat--relay-bindat-unsigned-to-signed
                               (bindat-get-field struct 'len)
                               1)))
                     ;; Hack for signed/unsigned problems
                     (if (<= len 0) 0 len))))))

(defun weechat--relay-unpack-tim (data)
  (let ((obj (bindat-unpack weechat--relay-tim-spec data)))
    (values (seconds-to-time
             (string-to-int
              (bindat-get-field obj 'val)))
            (bindat-length weechat--relay-tim-spec obj))))

(defconst weechat--relay-htb-spec
  '((key-type str 3)
    (val-type str 3)
    (count u32)))

(defun weechat--relay-unpack-htb (data)
  (let* ((obj (bindat-unpack weechat--relay-htb-spec data))
         (count (weechat--relay-bindat-unsigned-to-signed
                 (bindat-get-field obj 'count)
                 4))
         (key-type (bindat-get-field obj 'key-type))
         (val-type (bindat-get-field obj 'val-type))
         (key-fn (symbol-function (intern (concat "weechat--relay-unpack-" key-type))))
         (val-fn (symbol-function (intern (concat "weechat--relay-unpack-" val-type))))
         (offset (bindat-length weechat--relay-htb-spec obj))
         (acc ()))
    (dotimes (i count)
      (multiple-value-bind (key key-len) (funcall key-fn (substring data offset))
        (multiple-value-bind (val val-len) (funcall val-fn (substring data (+ offset key-len)))
          (setq acc (cons (cons key val) acc))
          (setq offset (+ offset key-len val-len)))))
    (values acc
            offset)))

(defun weechat--relay-parse-inf (data)
  (multiple-value-bind (name len) (weechat--relay-unpack-str data)
    (multiple-value-bind (value len*) (weechat--relay-unpack-str (substring data len))
      (values (cons name value)
              (+ len len*)))))

(defconst weechat--relay-inl-item-spec
  '((name struct weechat--relay-str-spec)
    (type str 3)))

(defun weechat--relay-parse-inl-item (data)
  (let* ((count (weechat--relay-bindat-unsigned-to-signed
                 (bindat-get-field
                  (bindat-unpack '((len u32)) data) 'len)
                 4))
         (offset 4)
         (acc ()))
    (while (< (length acc) count)
      (let* ((obj (bindat-unpack weechat--relay-inl-item-spec
                                 (substring data offset)))
             (fun (symbol-function (intern (concat "weechat--relay-unpack-"
                                                   (bindat-get-field obj 'type))))))
        (setq offset (+ offset (bindat-length weechat--relay-inl-item-spec obj)))
        (multiple-value-bind (value offset*) (funcall fun (substring data offset))
          (setq offset (+ offset offset*))
          (setq acc (cons
                     (cons (bindat-get-field obj 'name 'val) value)
                     acc)))))
    (values acc
            offset)))

(defconst weechat--relay-inl-spec
  '((name struct weechat--relay-str-spec)
    (count u32)))

(defun weechat--relay-parse-inl (data)
  (let* ((obj (bindat-unpack weechat--relay-inl-spec data))
         (acc ())
         (count (weechat--relay-bindat-unsigned-to-signed
                 (bindat-get-field obj 'count)
                 4))
         (offset (bindat-length weechat--relay-inl-spec obj)))
    (dotimes (i count)
      (multiple-value-bind (item offset*) (weechat--relay-parse-inl-item (substring data offset))
        (setq acc (cons item acc))
        (setq offset (+ offset offset*))))
    (values acc
            offset)))

(defun weechat--relay-parse-hda-item (h-path-length name-type-alist data)
  (let ((p-path ())
        (offset 0)
        (result ()))
    (dotimes (i h-path-length)
      (multiple-value-bind (el offset*) (weechat--relay-unpack-ptr (substring data offset))
        (setq p-path (cons el p-path))
        (setq offset (+ offset offset*))))
    (dolist (name-type name-type-alist)
      (let ((fun (symbol-function (intern (concat "weechat--relay-unpack-" (cdr name-type))))))
        (multiple-value-bind (obj offset*) (funcall fun (substring data offset))
          (setq result (cons (cons (car name-type) obj) result))
          (setq offset (+ offset offset*)))))
    (values (cons p-path result)
            offset)))

(defconst weechat--relay-hdh-spec
  '((h-path struct weechat--relay-str-spec)
    (keys struct weechat--relay-str-spec)
    (count u32)))

;;; from http://lists.gnu.org/archive/html/help-gnu-emacs/2009-06/msg00764.html
(defun weechat--partition-list (list length)
  (loop
   while list
   collect (subseq list 0 length)
   do (setf list (nthcdr length list))))

(defun weechat--hda-split-keys-string (str)
  (mapcar (lambda (x)
            (cons (car x)
                  (cadr x)))
          (weechat--partition-list (split-string str "[:,]") 2)))

(defun weechat--relay-parse-hda (data)
  (let* ((obj (bindat-unpack weechat--relay-hdh-spec data))
         (count (weechat--relay-bindat-unsigned-to-signed
                 (bindat-get-field obj 'count)
                 4))
         (name-type-alist (weechat--hda-split-keys-string
                           (bindat-get-field obj 'keys 'val)))
         (h-path-length (length (split-string (bindat-get-field obj 'h-path 'val) "[/]")))
         (offset (+ (bindat-length weechat--relay-hdh-spec obj)))
         (acc ()))
    (dotimes (i count)
      (multiple-value-bind (obj offset*) (weechat--relay-parse-hda-item
                                          h-path-length name-type-alist (substring data offset))
        (setq acc (cons obj acc))
        (setq offset (+ offset offset*))))
    (let ((h-path (bindat-get-field obj 'h-path 'val)))
      (values (list h-path acc)
              offset))))

(defconst weechat--relay-message-spec
  '((length u32)
    (compression u8)
    (id struct weechat--relay-str-spec)
    (data vec (eval (let ((l (- (bindat-get-field struct 'length)
                                4   ;length
                                1   ;compression
                                (+ 4 (length (bindat-get-field struct 'id 'val))))))
                      l)))))

(defun weechat--unpack-message-contents (data)
  (let* ((type (substring data 0 3))
         (fun (symbol-function (intern (concat "weechat--relay-parse-" type)))))
    (multiple-value-bind (obj len) (funcall fun (string-make-unibyte (substring data 3)))
      (values obj
              (+ len 3)))))

(defun weechat-unpack-message (message-data)
  "Unpack weechat relay message in `MESSAGE-DATA'.
Returns a list: (id data)."
  (let* ((msg (bindat-unpack weechat--relay-message-spec message-data))
         (data (concat (bindat-get-field msg 'data)))
         (msg-id (bindat-get-field msg 'id 'val))
         (ignore-msg (member msg-id weechat-relay-ignored-message-ids))
         (offset 0)
         (acc ()))
    ;; Only no-compression is supported atm
    (assert (eq 0 (bindat-get-field msg 'compression)))
    (when (not ignore-msg)
      (while (< offset (length data))
        (multiple-value-bind (obj offset*) (weechat--unpack-message-contents
                                            (substring data offset))
          (setq offset (+ offset offset*))
          (setq acc (cons obj acc)))))
    (values (cons msg-id (if ignore-msg '(ignored) acc))
            (bindat-get-field msg 'length))))

(defun weechat--message-available-p (&optional buffer)
  "Check if a weechat relay message available in `BUFFER'.
`buffer' defaults to current buffer."
  (with-current-buffer (get-buffer (or buffer
                                       weechat-relay-buffer-name))
    (and (> (buffer-size) 5)
         (>= (buffer-size)
             (bindat-get-field
              (bindat-unpack '((len u32))
                             (buffer-string))
              'len)))))

(defun weechat--relay-parse-new-message (&optional buffer)
  (with-current-buffer (get-buffer (or buffer
                                       weechat-relay-buffer-name))
    (when (weechat--message-available-p (current-buffer))
      (multiple-value-bind (ret len) (weechat-unpack-message
                                      (buffer-string))
        (delete-region (point-min) (+ (point-min) len))
        ret))))



(defun weechat-relay-get-id-callback (id)
  (cadr (assoc id weechat-relay-id-callback-alist)))

(defun weechat-relay-remove-id-callback (id)
  (let ((fun (weechat-relay-get-id-callback id)))
   (setq weechat-relay-id-callback-alist
         (delq (assoc id weechat-relay-id-callback-alist)
               weechat-relay-id-callback-alist))
   fun))

(defun weechat-relay-add-id-callback (id function &optional one-shot)
  (when (assoc id weechat-relay-id-callback-alist)
    (error "Id '%s' is already in `weechat-relay-id-callback-alist'." id))
  (let ((function* (if one-shot
                       (lambda (x)
                         (funcall function x)
                         (weechat-relay-remove-id-callback id))
                     function)))
    (setq weechat-relay-id-callback-alist (cons (list id function*)
                                                weechat-relay-id-callback-alist))))

(ert-deftest weechat-relay-id-callback ()
  (let ((weechat-relay-id-callback-alist nil))
   (let ((fun (lambda (xyz) nil)) )
     (weechat-relay-add-id-callback "23" fun)
     (should (equal fun (weechat-relay-get-id-callback "23")))
     (should (equal fun (weechat-relay-remove-id-callback "23"))))
   (setq weechat-relay-id-callback-alist nil)
   (should-error (progn (weechat-relay-add-id-callback "42" (lambda ()))
                        (weechat-relay-add-id-callback "42" (lambda ()))))))

(ert-deftest weechat-relay-id-callback-one-shot ()
  (let ((weechat-relay-id-callback-alist nil))
   (let ((fun (lambda (xyz) nil)) )
     (weechat-relay-add-id-callback "23" fun 'one-shot)
     (funcall (weechat-relay-get-id-callback "23") nil)
     (should (equal nil (weechat-relay-get-id-callback "23"))))))

(defun weechat--relay-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (string-make-unibyte string))
      (while (weechat--message-available-p)
        (let* ((data (weechat--relay-parse-new-message))
               (id (weechat--message-id data)))
          ;; If buffer is available, log message
          (weechat--relay-log (pp-to-string data))
          ;; Call `weechat-relay-message-function'
          (when (functionp weechat-relay-message-function)
            (funcall weechat-relay-message-function data))
          ;; Call callback from `weechat-relay-id-callback-alist'
          (if (functionp (weechat-relay-get-id-callback id))
              (funcall (weechat-relay-get-id-callback id) data)))))))

(defun weechat--relay-process-sentinel (proc msg)
  (let ((event (process-status proc)))
    (weechat--relay-log (format "Received event: %s\n" event))))

(defun weechat-relay-connect (host port)
  "Opens a new weechat relay connection to `HOST' at PORT `port'."
  (make-network-process :name "weechat-relay"
                        :buffer weechat-relay-buffer-name
                        :host host
                        :service port
                        :filter #'weechat--relay-process-filter
                        :sentinel #'weechat--relay-process-sentinel
                        :filter-multibyte nil
                        :coding 'binary)
  (get-buffer-create weechat-relay-log-buffer-name)
  (with-current-buffer (get-buffer weechat-relay-buffer-name)
    (read-only-mode 1)
    (set-buffer-multibyte nil)))

(defun weechat-relay-connected-p ()
  (and (get-buffer weechat-relay-buffer-name)
       (get-buffer-process weechat-relay-buffer-name)
       (process-live-p (get-buffer-process weechat-relay-buffer-name))
       t))

(defun weechat-relay-disconnect ()
  "Disconnect current weechat relay connection and close all
buffers."
  (when (weechat-relay-connected-p)
    (weechat--relay-send-message "quit")
    (with-current-buffer weechat-relay-buffer-name
      (delete-process
       (get-buffer-process (current-buffer)))
      (kill-buffer))
    (when (get-buffer weechat-relay-log-buffer-name)
      (kill-buffer weechat-relay-log-buffer-name))))

(defun weechat--message-id (message)
  (car message))

(defun weechat--message-data (message)
  (cadr message))

(ert-deftest weechat-test-message-fns ()
  (let ((message '("42" ("version" . "0.3.8"))))
    (should (equal "42" (weechat--message-id message)))
    (should (equal '("version" . "0.3.8") (weechat--message-data message)))))

(defun weechat--hdata-path (hdata)
  (car hdata))

(defun weechat--hdata-values (hdata)
  (cadr hdata))

(defun weechat--hdata-value-pointer-path (value)
  (car value))

(defun weechat--hdata-value-cons (value)
  (cadr value))

(ert-deftest weechat-test-hdata-fns ()
  (let ((hdata '("foo" ((("114b240") ("full_name" . "irc.server.euirc"))
                        (("10ea8d0") ("full_name" . "irc.server.freenode"))
                        (("10967c0") ("full_name" . "core.weechat"))))))
    (should (equal "foo" (weechat--hdata-path hdata)))
    (should (listp (weechat--hdata-values hdata)))
    (should (equal '(("114b240") ("10ea8d0") ("10967c0"))
                   (mapcar #'weechat--hdata-value-pointer-path (weechat--hdata-values hdata))))
    (should (equal '(("full_name" . "irc.server.euirc")
                     ("full_name" . "irc.server.freenode")
                     ("full_name" . "core.weechat"))
                   (mapcar #'weechat--hdata-value-cons (weechat--hdata-values hdata))))))

;;; Various tests

(ert-deftest weechat-test-infolist ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-string (concat [0 0 0 32 0 255 255 255 255 105 110 102 0 0
                              0 7 118 101 114 115 105 111 110 0 0 0 5
                              48 46 51 46 56]))
    (let ((data (weechat--relay-parse-new-message (current-buffer))))
      (should (equal ""  (weechat--message-id data)))
      (should (equal '("version" . "0.3.8")
                     (weechat--message-data data))))))

(ert-deftest weechat-test-id ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-string (concat [0 0 0 35 0 0 0 0 3 54 54 54 105 110 102 0
                              0 0 7 118 101 114 115 105 111 110 0 0 0
                              5 48 46 51 46 56]))
    (let ((data (weechat--relay-parse-new-message (current-buffer))))
      (should (equal "666" (weechat--message-id data)))
      (should (equal '("version" . "0.3.8")
                     (weechat--message-data data))))))

(ert-deftest weechat-relay-test-connection ()
  (when (weechat-relay-connected-p)
    (let ((info-data nil)
          (info-id (symbol-name (gensym))))
      (weechat-relay-add-id-callback info-id (lambda (data) (setq info-data data)) t)
      (weechat--relay-send-message "info version" info-id)
      (sleep-for 1)
      (should (equal info-id (weechat--message-id info-data)))
      (should (equal "version" (car (weechat--message-data info-data)))))))

(provide 'weechat-relay)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; weechat-relay.el ends here

