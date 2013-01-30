;; -*- lexical-binding: t -*-

(require 'weechat-relay)
(require 'ert)

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
     (puthash (car alist) (cdr alist) hash))
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
   "hdata buffer:gui_buffers(*) name,short_name,title,number"
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
