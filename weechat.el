;; -*- lexical-binding: t -*-

(require 'weechat-relay)
(require 'ert)

(defvar weechat--buffer-hash (make-hash-table :test 'equal))

(defun weechat-buffer-alist (buffer-ptr)
  (gethash buffer-ptr weechat--buffer-hash))

(defun weechat--clear-buffer-store ()
  (clrhash weechat--buffer-hash))

(defun weechat--store-buffer-alist (ptr val)
  (when (weechat-buffer-alist ptr)
    (error "Buffer '%s' already exists" ptr))
  (puthash ptr val weechat--buffer-hash))

(defun weechat--remove-buffer-alist (ptr)
  (when (not (weechat-buffer-alist ptr))
    (error "Buffer '%s' doesn't exist" ptr))
  (remhash ptr weechat--buffer-hash))

(ert-deftest weechat-test-buffer-store ()
  (let ((weechat--buffer-hash (make-hash-table :test 'equal)))
    (weechat--clear-buffer-store)
    (should (eql 0 (hash-table-count weechat--buffer-hash)))
    (weechat--store-buffer-alist "0xffffff" '(("name" . "#asimov")))
    (should (equal '(("name" . "#asimov"))
                   (weechat-buffer-alist "0xffffff")))
    (weechat--remove-buffer-alist "0xffffff")
    (should (not (weechat-buffer-alist "0xffffff")))))

(defun weechat--handle-buffer-list (hdata)
  (weechat--clear-buffer-store)
  (dolist (value (weechat--hdata-values hdata))
    (let ((buffer-ptr (car (weechat--hdata-value-pointer-path value))))
      (weechat--store-buffer-alist
       buffer-ptr
       (weechat--hdata-value-alist value)))))

(defun weechat-update-buffer-list ()
  (weechat-relay-send-command
   "hdata buffer:gui_buffers(*) name,short_name,title"
   #'weechat--handle-buffer-list))

(defun weechat--handle-buffer-opened (hdata)
  (let* ((value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value))))
    (when (weechat-buffer-alist buffer-ptr)
      (error "Received '_buffer_opened' event for '%s' but the buffer exists already!" buffer-ptr))
    (weechat--store-buffer-alist buffer-ptr (weechat--hdata-value-alist value))))

(defun weechat--handle-buffer-closed (hdata)
  (let* ((value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value))))
    (when (not (weechat-buffer-alist buffer-ptr))
      (error "Received '_buffer_closed' event for '%s' but the buffer doesn't exist" buffer-ptr))
    (weechat--remove-buffer-alist buffer-ptr)))

(defun weechat--handle-buffer-renamed (hdata)
  (let (buf)))

(weechat-relay-add-id-callback "_buffer_opened" #'weechat--handle-buffer-opened nil 'force)
(weechat-relay-add-id-callback "_buffer_closing" #'weechat--handle-buffer-closed nil 'force)

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

(provide 'weechat)

