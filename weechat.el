;; -*- lexical-binding: t -*-

(require 'weechat-relay)

(defvar weechat--buffer-alist '())

(defun weechat--handle-buffer-list (hdata)
  (setq weechat--buffer-alist ())
  (dolist (value (weechat--hdata-values hdata))
    (let ((buffer-ptr (car (weechat--hdata-value-pointer-path value))))
      (setq weechat--buffer-alist
            (cons (cons buffer-ptr (weechat--hdata-value-alist value)) weechat--buffer-alist)))))

(defun weechat-update-buffer-list ()
  (weechat-relay-send-command
   "hdata buffer:gui_buffers(*) name,short_name,title"
   #'weechat--handle-buffer-list))

(defun weechat-connect (host port password)
  (interactive (list (read-string "Relay Host: ")
                     (read-number "Port: ")
                     (read-passwd "Password: ")))
  (when (and (stringp host)
             (integerp port))
    (weechat-relay-connect host port)
    (weechat-relay-authenticate password)
    (weechat-relay-send-command
     "info version"
     (lambda (data)
       (message "Connected to '%s', version %s" host (cdr data))
       (weechat-update-buffer-list)))))

(defun weechat-disconnect ()
  (interactive)
  (weechat-relay-disconnect)
  (setq weechat--buffer-alist nil))

(add-hook 'weechat-relay-disconnect-hook (lambda () (message "Disconnected from Weechat")))

(provide 'weechat)
