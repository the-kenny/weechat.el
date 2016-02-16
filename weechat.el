;;; weechat --- Chat via WeeChat's relay protocol in Emacs ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Moritz Ulrich

;; Author: Moritz Ulrich <moritz@tarn-vedra.de>
;;         Rüdiger Sonderfeld <ruediger@c-plusplus.de>
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

;; This package provides a way to chat via WeeChat's relay protocol in
;; Emacs.

;; Please see README.org on how to use it.

;;; Code:

(require 'weechat-core)
(require 'weechat-relay)
(require 'weechat-color)
(require 'cl-lib)
(require 'format-spec)
(require 's)

(defcustom weechat-host-default "localhost"
  "Default host for `weechat-connect'."
  :type 'string
  :group 'weechat)

(defcustom weechat-port-default 9000
  "Default port for `weechat-connect'."
  :type 'string
  :group 'weechat)

(defcustom weechat-mode-default 'plain
  "Wether to connect via SSL by default.

Null or 'plain: Plain socket.
t or 'ssl: TLS socket.
String: command to run."
  :type '(choice
          (const :tag "Plain" 'plain)
          (const :tag "SSL/TLS" 'ssl)
          (string :tag "Command to run"))
  :group 'weechat)

(defcustom weechat-modules '(weechat-button weechat-complete)
  "Modules loaded when weechat.el is loaded.

Each module must be in `load-path' and must have a call to
provide in order to be loaded correctly.

To unload modules, use (unload-feature FEATURE)."
  :type '(repeat symbol)
  :group 'weechat)

(defcustom weechat-read-only t
  "Whether to make text in weechat buffers read-only."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-initial-lines 100
  "Number of lines to show when initializing a channel buffer."
  :type 'integer
  :group 'weechat)

(defcustom weechat-more-lines-amount 10
  "Number of extra lines `weechat-get-more-lines' will retieve."
  :type 'integer
  :group 'weechat)

(defcustom weechat-prompt "[%n] "
  "The Weechat prompt."
  :type 'string
  :group 'weechat)

(defcustom weechat-buffer-line-limit 1000
  "Number of max.  lines per buffer."
  :type '(choice integer
                 (const :tag "Unlimited" nil))
  :group 'weechat)

(defcustom weechat-return-always-replace-input t
  "Always replace current input with line on return.

If set to t, pressing return will always copy the current line to
the input prompt.  If nil, only copy when the input line is
empty."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-auto-move-cursor-to-prompt t
  "Automatically move the cursor to the prompt when typing."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-auto-recenter t
  "Wether the prompt will always stay at the bottom"
  :type 'boolean
  :group 'weechat)

(defcustom weechat-hidden-text-hidden t
  "Wether weechat.el should hide or show hidden text. "
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

(defcustom weechat-auto-reconnect-retries 5
  "Number of max. retries when reconnecting"
  :type '(choice (integer :tag "Number of retries")
                 (const :tag "No auto-reconnect" nil))
  :group 'weechat)

(defcustom weechat-auto-monitor-buffers ()
  "List of buffer names to auto-monitor on connect.

If value is a list, buffers corresponding the names will be
monitored on connect. If value is a string, monitor all buffers
matching the string as regexp. A value of t will monitor all
available buffers. Be warned, a too long list will use much
bandwidth on connect."
  :type '(choice (const :tag "All" t)
                 (repeat :tag "List" string)
                 string)
  :group 'weechat)

(defcustom weechat-auto-monitor-new-buffers 'silent
  "Wether to auto-monitor new WeeChat buffers.

Value can be t, silent or nil.  If t, new Emacs buffers will be
created when a new buffer in WeeChat is opened.  If value
is (quote silent), new buffers will be opened in background.  If
nil, no action will be taken for new WeeChat buffers."
  :type '(choice (const :tag "Popup new buffer" t)
                 (const :tag "Open in background" 'silent)
                 (const :tag "Do nothing" nil))
  :group 'weechat)

(defcustom weechat-monitor-buffer-function 'message
  "Function called when a new buffer is monitored. Useful to
  display notifications.

Value is either 'message or a function taking one argument (a
buffer-ptr). "
  :type '(choice (const :tag "Message in the mode line" 'message)
                 hook)
  :group 'weechat)

(defcustom weechat-auto-close-buffers nil
  "Wether to auto-close closed WeeChat buffers."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-time-format "%H:%M:%S"
  "How to format time stamps.
See `format-time-string' for format description."
  :type 'string
  :group 'weechat)

(defcustom weechat-text-column 22
  "Column after which text will be inserted.
If `(length (concat nick timestamp))' is longer than this value,
text-column will be increased for that line."
  :type 'integer
  :group 'weechat)

(defcustom weechat-max-nick-length nil
  "Maximum length of nicknames. Longer nicks will be truncated.

Note that this option will apply to all prefixes, not just
nicknames."
  :type '(choice
          (integer :tag "Max length")
          (const :tag "Off" nil))
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
          (const :tag "Default" t))
  :group 'weechat)

(defcustom weechat-notification-mode :monitored
  "When to notify the user.

Possible values are nil (Never), :monitored (Only monitored
buffers) and t (All buffers)."
  :type '(choice
          (const :tag "Never" nil)
          (const :tag "Monitored buffers" :monitored)
          (const :tag "All Buffers" t))
  :group 'weechat)

(defcustom weechat-notification-types '(:highlight :disconnect :query)
  "Events for which a notification should be shown."
  :type '(repeat symbol)
  :group 'weechat)

(defcustom weechat-header-line-format "%n on %c/%s: %t"
  "Header line format.
Set to nil to disable header line.  Supported options are:

- %n nick name
- %s server name
- %c channel name
- %N buffer name
- %t topic"
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

(defcustom weechat-insert-modify-hook nil
  "The hook will be called after new text is inserted into the buffer.
It is called with narrowing in the correct buffer."
  :type 'hook
  :group 'weechat)

(defcustom weechat-message-post-receive-functions nil
  "List of function called after a new line was received for a buffer.

This hook is useful in conjunction with
`weechat-last-background-message-date' or
`weechat-last-background-highlight-date'.

Functions must take one argument: The buffer-ptr.

If the weechat-buffer is currently associated with an emacs
buffer, the functions will get called with the active buffer set
to it."
  :type 'hook
  :group 'weechat)

(defcustom weechat-message-filter-functions nil
  "List of functions called in sequence before a line will be sent to the server.

The functions (a b c) are applied like (a (b (c input-string))).
If a function returns nil, evaluation will stop and the line is
ignored."
  :type '(repeat :tag "List" function)
  :group 'weechat)

(defcustom weechat-message-filter-require-double-ret nil
  "If t, pressing RET once will show the result of the filter.

If nil, filtering will take place transparently when sending the
message."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-complete-order-nickname t
  "If non-nil nicknames are completed in order of most recent speaker."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-password-callback 'weechat-password-auth-source-callback
  "Function called to get the relay password.  Set to nil if no
  password is needed.

Value must be a function with two arguments: Hostname and port.
The return value must be either a string, a function which
returns a string, or nil."
  :type 'function
  :group 'weechat)

(defcustom weechat-buffer-activity-types '(:irc/privmsg :irc/action :irc/notice)
  "List of types which will contribute to buffer activity."
  :type '(repeat :tag "List" symbol)
  :group 'weechat)

(defcustom weechat-buffer-kill-buffers-on-disconnect nil
  "Kill buffers if the connection is disconnected by the user."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-sync-active-buffer nil
  "Sync currently visible buffer with the relay (one-way).

When set to t, weechat.el will switch the currently active
weechat buffer on the relay server when visiting a buffer in
weechat.

This is useful when setting irc.msgbuffer.* to 'current'.

Syncing is done when sending a command/message to the buffer."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-sync-buffer-read-status nil
  "Mark buffers as read in the relay, when read with weechat.el.

When set to t, weechat will automatically mark buffers as read in
the relay, when they are visited in the client.

Be aware, that this setting can loose highlights: If the
highlight occurred more than `weechat-initial-lines' before Emacs
connects to the relay, reading the Emacs buffer will not show the
highlight, but mark the buffer as read.

Also, weechat >= 1.0 is required for this to work."
  :type 'boolean
  :group 'weechat)

(defcustom weechat-completing-read-function 'weechat--try-ido
  "Function to prompt for channel names.

The function must comply to the interface of `completing-read'.

Possible choices would be `ido-completing-read' or
`completing-read'."
  :type '(choice
          (const :tag "Ido" weechat--try-ido)
          (const :tag "Ivy" weechat--try-ivy)
          (const :tag "Default" completing-read)
          (function :tag "Other"))
  :group 'weechat)

(defvar weechat--buffer-hashes (make-hash-table :test 'equal))

(defvar weechat--connected nil)

(defvar weechat-host-history nil
  "List of recently connected hosts.")
(defvar weechat-last-port nil
  "Last port connected to.")
(defvar weechat-mode-history nil
  "List of recently used connection modes.")

(defvar weechat-version nil)
(add-to-list 'version-regexp-alist '("^[-_+ ]dev$" . -3))

(defvar weechat-buffer-opened-functions nil
  "Hook ran when a WeeChat buffer opens.")

(defvar weechat-buffer-closed-functions nil
  "Hook ran when a WeeChat buffer closes.")

(defvar weechat-notification-handler-functions nil
  "List of functions called to display notificiations.

The functions are called with the following arguments:

TYPE, a symbol from `weechat-notification-types' Other optional
arguments are SENDER, TEXT, DATE, and BUFFER-PTR.")

(defvar weechat-inhibit-notifications nil
  "Non-nil means don't display any weechat notifications.")

(defun weechat-load-modules-maybe ()
  "Load all modules listed in `weechat-modules'"
  ;; Inspired by `org-load-modules-maybe'
  (dolist (module weechat-modules)
    (condition-case nil (load-library (symbol-name module))
      (error (weechat-warn "Problems while trying to load feature `%s'" module)))))

;;; This is a hack to load modules after weechat.el is loaded
;;; completely
(eval-after-load 'weechat
  '(weechat-load-modules-maybe))

;;; Add all hooks ending in -functions to
;;; `unload-feature-special-hooks' to make `unload-feature' remove the
;;; hooks on unload

(eval-after-load 'loadhist
  '(setq unload-feature-special-hooks
         (append unload-feature-special-hooks
                 '(weechat-buffer-opened-functions
                   weechat-buffer-closed-functions
                   weechat-notification-handler-functions
                   weechat-message-post-receive-functions))))

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
      (error "Received '_buffer_opened' event for '%s' but the buffer exists already" buffer-ptr))
    (weechat--store-buffer-hash buffer-ptr (weechat--hdata-value-alist value))

    (when weechat-auto-monitor-new-buffers
      (weechat-monitor-buffer
       buffer-ptr
       (not (eq weechat-auto-monitor-new-buffers 'silent))))

    (run-hook-with-args 'weechat-buffer-opened-functions
                        buffer-ptr)))

(defun weechat--handle-buffer-closed (response)
  (let* ((hdata (car response))
         (value (car (weechat--hdata-values hdata)))
         (buffer-ptr (car (weechat--hdata-value-pointer-path value)))
         (emacs-buffer (weechat--emacs-buffer buffer-ptr)))
    (unless (weechat-buffer-hash buffer-ptr)
      (error "Received '_buffer_closed' event for '%s' but the buffer doesn't exist" buffer-ptr))
    (when (buffer-live-p emacs-buffer)
      ;; Add text about quitting etc. bla
      (weechat-print-line buffer-ptr :prefix "Buffer closed")
      ;; Close buffer if user wants this
      (when weechat-auto-close-buffers
        (kill-buffer emacs-buffer)))
    ;; Remove from buffer hash map
    (weechat--remove-buffer-hash buffer-ptr)
    ;; Finally, run hook
    (run-hook-with-args 'weechat-buffer-closed-functions
                        buffer-ptr)))

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

;;; Handle pong replies
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

(defun weechat-password-auth-source-callback (host port)
  "Get password for HOST and PORT via `auth-source-search'.
Returns either a string or a function.  See Info node `(auth) Top' for details."
  (when (fboundp 'auth-source-search)
    (weechat-message "Using auth-source to retrieve weechat relay password")
    (plist-get
     (car (auth-source-search
           :max 1
           :host host
           :port port
           :require '(:secret)))
     :secret)))

(defun weechat-get-password (host port)
  "Get password for HOST and PORT.
Return either a string, a function returning a string, or nil."
  (when (functionp weechat-password-callback)
    (funcall weechat-password-callback host port)))

(defvar weechat-mode-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'minibuffer-complete)
    (define-key map "?" 'minibuffer-completion-help)
    map)
  "Weechat mode selection: Local keymap for minibuffer input with completion.")

(defvar weechat-reconnect-timer nil)
(defun weechat-cancel-reconnect ()
  (when (timerp weechat-reconnect-timer)
    (cancel-timer weechat-reconnect-timer)
    (setq weechat-reconnect-timer nil))
  (unintern 'weechat-auto-reconnect-retries-left obarray))

;;;###autoload
(defun weechat-connect (&optional host port password mode force-disconnect)
  "Connect to WeeChat.

HOST is the relay host, `weechat-host-default' by default.
PORT is the port where the relay listens, `weechat-port-default' by default.
PASSWORD is either a string, a function or nil.
MODE is null or 'plain for a plain socket, t or 'ssl for a TLS socket;
a string denotes a command to run.  You can use %h and %p to interpolate host
and port number respectively."
  (interactive
   (let* ((host
           (read-string
            (format "Relay host (default '%s'): " weechat-host-default)
            nil 'weechat-host-history weechat-host-default))
          (port
           (read-number "Port: " (or weechat-last-port weechat-port-default)))
          (mode (let*
                    ((minibuffer-local-completion-map weechat-mode-completion-map)
                     (modestr (completing-read
                               (format "Mode (`plain', `ssl' or command, default `%s'): "
                                       weechat-mode-default)
                               '("plain" "ssl" "ssh -W localhost:%p %h")
                               nil nil nil 'weechat-mode-history
                               ;; NOTE: `completing-read' is fine when
                               ;; passed a symbol, but helm breaks.
                               ;; The following ensures we always pass
                               ;; a string.
                               (format "%s" weechat-mode-default))))
                  (cond
                   ((string= modestr "") nil)
                   ((string= modestr "plain") 'plain)
                   ((string= modestr "ssl") 'ssl)
                   (t modestr)))))
     (setq weechat-last-port port)
     (list
      host port
      (or
       (progn
         (weechat-message "Trying to get password via `weechat-password-callback'...")
         (weechat-get-password host port))
       ;; Use lexical-let to scramble password lambda in *Backtrace*
       (read-passwd "Password: "))
      mode
      nil)))
  ;; Cancel the reconnect timer to prevent surprises
  (weechat-cancel-reconnect)
  ;; Handle when the user is already connected etc.
  (let* ((host (or host weechat-host-default))
         (port (or port weechat-port-default))
         (password (or password
                       (weechat-get-password host port)))
         (mode (or mode weechat-mode-default)))
    (weechat-message "Weechat connecting to %s:%d" host port)
    (when (weechat-relay-connected-p)
      (if (or force-disconnect
              (y-or-n-p "Already connected.  Disconnect other connection? "))
          (weechat-relay-disconnect)
        (error "Can't open two connections")))
    (when (and (stringp host)
               (integerp port))
      (weechat-relay-connect
       host
       port
       mode
       (lambda ()
         (weechat-relay-authenticate password)
         (weechat-relay-send-command
          "info version"
          (lambda (data)
            (let ((version-str (cdar data)))
              (weechat-message "Connected to '%s', version %s" host
                               version-str)
              (setq weechat-version version-str))
            (weechat-update-buffer-list
             (lambda ()
               (weechat-relay-send-command "sync")
               (setq weechat--connected t)
               (weechat--relay-start-ping-timer)
               (weechat-cancel-reconnect)
               (run-hooks 'weechat-connect-hook))))))))))

(defvar weechat-auto-reconnect-retries-left)
(defun weechat-handle-reconnect-maybe ()
  (weechat-cancel-reconnect)
  (unless (boundp 'weechat-auto-reconnect-retries-left)
    (setq weechat-auto-reconnect-retries-left
          weechat-auto-reconnect-retries))
  (when (> weechat-auto-reconnect-retries-left 0)
    (let ((host (car weechat-host-history))
          (port weechat-last-port)
          (delay (lsh 1 (- weechat-auto-reconnect-retries
                           weechat-auto-reconnect-retries-left))))
      (if (not (weechat-get-password host port))
          (weechat-message "Not reconnecting: No password stored.")
        (weechat-message "Reconnecting in %ds..." delay)
        (cl-decf weechat-auto-reconnect-retries-left)
        (setq weechat-reconnect-timer
              (run-with-timer
               delay nil
               (lambda ()
                 (weechat-connect
                  host
                  port
                  (weechat-get-password host port)
                  (car weechat-mode-history)
                  'force-disconnect)))))
      t)))

(defun weechat-handle-disconnect ()
  (setq weechat--connected nil
        weechat-version nil)
  (unless (and weechat-auto-reconnect-retries
               (weechat-handle-reconnect-maybe))
    ;; Print 'disconnected' message to all channel buffers
    (maphash (lambda (k v)
               (when (bufferp (gethash :emacs/buffer v))
                 (with-current-buffer (gethash :emacs/buffer v)
                   (weechat-print-line k
                                       :prefix "!!!"
                                       :text "Lost connection to relay server"
                                       :date (current-time)
                                       :line-type :irc/x-error))))
             weechat--buffer-hashes)
    (weechat-notify :disconnect
                    :date (current-time))))

(defun weechat-disconnect ()
  (interactive)
  ;; It's safe to lexical-bind the retry limit to nil to disable
  ;; reconnects
  (let ((weechat-auto-reconnect-retries nil))
    ;; Disconnect the relay. `weechat-relay-disconnect-hook' will NOT
    ;; run.
    (weechat-relay-disconnect)
    (weechat-handle-disconnect)
    (when weechat-buffer-kill-buffers-on-disconnect
      (weechat-do-buffers (kill-buffer)))
    (clrhash weechat--buffer-hashes)
    (setq weechat--connected nil)))

(add-hook 'weechat-relay-disconnect-hook 'weechat-handle-disconnect)

(defun weechat-buffer-name (buffer-ptr)
  (let ((hash (weechat-buffer-hash buffer-ptr)))
    (or (gethash "name"        hash)
        (gethash "full_name"   hash)
        ;; NOTE: Short name isn't useful to identify the buffer
        ;; (gethash "short_name" hash)
        )))

(defun weechat--find-buffer (name)
  "Return buffer-ptr for channel NAME."
  (let (ret)
    (maphash
     (lambda (ptr _)
       (when (string= name (weechat-buffer-name ptr))
         (setq ret ptr)))
     weechat--buffer-hashes)
    ret))

(defun weechat--channel-names-pred (l r)
  "Compare channel name L and R.
Return non-nil if L < R.  Names of actual channels should come first."
  (let ((l?  (s-contains? "#" l))
        (r?  (s-contains? "#" r))
        (l<r? (string< l r)))
    (or (and l? l<r?)
        (and l? (not r?))
        (and (not r?) l<r?))))

(defun weechat-channel-names (&optional arg sort)
  "Return all available buffer names in WeeChat.

If ARG is non-nil, only return monitored buffers.  If SORT is non-nil then sort
the channel list with actual channels coming first."
  (let (ret)
    (maphash
     (lambda (k v)
       (when (or (not arg) (buffer-live-p (gethash :emacs/buffer v)))
         (setq ret (cons (weechat-buffer-name k) ret))))
     weechat--buffer-hashes)
    (if sort
        (sort ret #'weechat--channel-names-pred)
      ret)))

(defun weechat-buffer-list ()
  "List all Weechat buffers."
  (let (acc)
    (maphash (lambda (_ v)
               (when (buffer-live-p (gethash :emacs/buffer v))
                 (setq acc (cons (gethash :emacs/buffer v) acc))))
             weechat--buffer-hashes)
    acc))

(defmacro weechat-do-buffers (&rest body)
  "Evaluate body in each WeeChat buffer."
  `(maphash (lambda (_ v)
              (let ((buffer (gethash :emacs/buffer v)))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    ,@body))))
            weechat--buffer-hashes))

(defun weechat-channel-names-unmonitored ()
  (cl-remove-if
   (lambda (name)
     (weechat--emacs-buffer
      (weechat--find-buffer name)))
   (weechat-channel-names)))

(defun weechat--emacs-buffer (buffer-ptr)
  (let ((hash (gethash buffer-ptr weechat--buffer-hashes)))
    (gethash :emacs/buffer hash)))

(defun weechat-visible-buffers (&optional current-frame-only)
  "Returns list of all visible weechat.el channel buffers.

Optional argument CURRENT-FRAME-ONLY limits list to current
frame."
  (let (ret)
    (weechat-do-buffers
     (when (window-live-p (get-buffer-window
                           (current-buffer)
                           (not current-frame-only)))
       (setq ret (cons (current-buffer) ret))))
    ret))

;;; Buffer local variables
(defvar weechat-buffer-ptr)
(defvar weechat-server-buffer)
(defvar weechat-buffer-number)
(defvar weechat-local-prompt)
(defvar weechat-lines-received)

;;; The following functions handle buffer-hash-entries storing the
;;; last highlight and the last message. The entries will be cleared
;;; automatically when the buffer becomes visible. This is useful for
;;; a mode-line display of modified buffers etc.

(defvar weechat-buffer-background-message-hook nil
  "Hook called when a message was received in a weechat buffer
  which isn't currently visible. Called with the corresponding
  buffer active.")
(defvar weechat-buffer-background-highlight-hook nil
  "Same as `weechat-buffer-background-message-hook', only for highlights.")
(defvar weechat-buffer-visited-hook nil
  "Hook called when a weechat-buffer is visited and the
  background-data is reset.")

(defun weechat--reset-relay-read-status (buffer-ptr)
  "Mark the buffer BUFFER-PTR as read in the relay."
  (when (and weechat-version
             (not (version< weechat-version "1.0"))
             weechat-sync-buffer-read-status)
    (weechat-relay-send-command (concat "input " buffer-ptr " /buffer set hotlist -1"))
    (weechat-relay-send-command (concat "input " buffer-ptr " /input set_unread_current_buffer"))))

(defun weechat-reset-buffer-modified (buffer-ptr)
  (let ((hash (weechat-buffer-hash buffer-ptr)))
    (when (hash-table-p hash)
      (weechat--reset-relay-read-status buffer-ptr)
      (run-hooks 'weechat-buffer-visited-hook)
      (remhash :background-message hash)
      (remhash :background-highlight hash))))

(defun weechat-buffer-modified-update-hash (hash line-data)
  (let ((hash (or hash (make-hash-table))))
    (puthash :date (assoc-default "date" line-data) hash)
    (puthash :sender (weechat--get-nick-from-line-data line-data) hash)
    (puthash :count (1+ (gethash :count hash 0)) hash)
    hash))

(defun weechat-update-buffer-modified (buffer-ptr line-data)
  (let ((line-type (weechat-line-type line-data))
        (line-date (assoc-default "date" line-data))
        (nick (weechat--get-nick-from-line-data line-data))
        (hash (weechat-buffer-hash buffer-ptr))
        (emacs-buffer (weechat--emacs-buffer buffer-ptr)))
    (unless (hash-table-p hash)
      (error "Tried to update modification date for unknown buffer-ptr '%s'" buffer-ptr))
    (if (and (buffer-live-p emacs-buffer)
             (cl-find emacs-buffer (weechat-visible-buffers) :test 'equal))
        ;; Buffer is visible. Reset modification
        (weechat-reset-buffer-modified buffer-ptr)
      ;; Buffer invisible. Store modifications.
      (when (and line-type line-date nick)
        (cond
         ;; Message from ourself. Reset.
         ((string= nick (weechat-get-local-var "nick" buffer-ptr))
          (weechat-reset-buffer-modified buffer-ptr))
         ;; General activity
         ((memq line-type weechat-buffer-activity-types)
          (puthash :background-message
                   (weechat-buffer-modified-update-hash
                    (gethash :background-message hash)
                    line-data)
                   hash)
          (when (buffer-live-p emacs-buffer)
            (with-current-buffer emacs-buffer
              (run-hooks 'weechat-buffer-background-message-hook)))))
        ;; Highlight
        (when (eq 1 (cdr (assoc-string "highlight" line-data)))
          (puthash :background-highlight
                   (weechat-buffer-modified-update-hash
                    (gethash :background-highlight hash)
                    line-data)
                   hash)
          (when (buffer-live-p emacs-buffer)
            (with-current-buffer emacs-buffer
              (run-hooks 'weechat-buffer-background-highlight-hook))))))))

(defun weechat-window-configuration-change ()
  "Resets modification dates for all visible buffers."
  (dolist (b (weechat-visible-buffers))
    (with-current-buffer b
      (weechat-reset-buffer-modified weechat-buffer-ptr))))

(add-hook 'window-configuration-change-hook 'weechat-window-configuration-change)

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

(cl-defun weechat-notify (type &key sender text date buffer-ptr)
  (when (and (memq type weechat-notification-types)
             (or (eq weechat-notification-mode t)
                 (and (eql weechat-notification-mode :monitored)
                      (local-variable-p 'weechat-buffer-ptr)
                      (buffer-live-p (weechat--emacs-buffer weechat-buffer-ptr)))))
    (dolist (fn weechat-notification-handler-functions)
      (with-demoted-errors
        (funcall fn type sender text date buffer-ptr)))))

(defun weechat-buffer-p (&optional buffer)
  "Return non-nil if buffer is a WeeChat buffer."
  (eq 'weechat-mode (buffer-local-value 'major-mode
                                        (or buffer (current-buffer)))))

(defun weechat-narrow-to-line ()
  (interactive)
  (unless (weechat-buffer-p)
    (error "No weechat-mode buffer"))
  (when (> (point) weechat-prompt-start-marker)
    (error "Only narrowing to lines is supported"))
  (narrow-to-region (point-at-bol) (min (point-at-eol)
                                        weechat-prompt-start-marker)))

(defun weechat-truncate-buffer ()
  (when (integerp weechat-buffer-line-limit)
    (save-excursion
      (save-restriction
        (widen)
        (let ((lines-to-delete (- (- weechat-buffer-line-limit
                                     (count-lines (point-min) (point-max)))))
              (inhibit-read-only t))
          (when (> lines-to-delete 0)
            (goto-char (point-min))
            (forward-line lines-to-delete)
            (delete-region (point-min) (point))))))))

(defun weechat-line-add-properties (nick date highlight invisible)
  "Add various text properties (read-only, etc.) to a line.

Must be called with `weechat-narrow-to-line' active."
  ;; Add `date' and `highlighted' to the whole line
  (add-text-properties (point-min) (point-max)
                       (list 'weechat-nick nick
                             'weechat-date date
                             'weechat-highlighted highlight))

  ;; Make line read-only if `weechat-read-only' is t
  (when weechat-read-only
    (add-text-properties (point-min) (point-max)
                         '(read-only t))))

(defun weechat-recenter-bottom-maybe (&optional window force)
  (when weechat-auto-recenter
    (let ((window (or (windowp window) (get-buffer-window))))
      (when window
        (with-selected-window window
          (when (weechat-buffer-p)
            (when (or force
                      (<= (- (window-body-height)
                             (count-screen-lines (window-point)
                                                 (window-start))
                             2)         ;2, not 1 (like in rcirc)
                                        ;because of the header-line
                          0))
              (recenter -1))))))))

(defun weechat-line-date ()
  "Return the date of the line under point."
  (get-text-property (point) 'weechat-date))

(defun weechat-line-nick ()
  "Return the nickname of the line under point."
  (get-text-property (point) 'weechat-nick))

(defun weechat-line-text-start ()
  "Return position where line text (message etc.) starts.

Might return the value of (point-at-eol) when there's no
text (technically, this shouldn't happen)."
  (next-single-property-change (point-at-bol)
                               'weechat-text
                               nil
                               (point-at-eol)))

(defun weechat-line-text ()
  (save-excursion
    (let ((start (weechat-line-text-start)))
      (when (< start (point-at-eol))
        (buffer-substring start (point-at-eol))))))

(cl-defun weechat-print-line (buffer-ptr &key prefix text date line-type highlight invisible nick)
  (setq text   (or text ""))
  (setq prefix (or prefix ""))
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

            (weechat-narrow-to-line)

            (when date
              (insert (propertize
                       (format-time-string weechat-time-format date)
                       'face 'weechat-time-face)
                      " "))

            (unless (s-blank? (weechat-handle-color-codes prefix))
              (let ((colorized-prefix (weechat-handle-color-codes prefix)))
                (insert (if (and (integerp weechat-max-nick-length)
                                 (> weechat-max-nick-length 0))
                            (substring colorized-prefix 0
                                       (min (length colorized-prefix)
                                            weechat-max-nick-length))
                          colorized-prefix)))
              (when (or (eq line-type :irc/privmsg)
                        (not line-type))
                (insert ":")))

            (let ((chars-to-insert
                   (- weechat-text-column
                      (- (point-max) (point-min)))))
              (when (> chars-to-insert 0)
                (insert-char ?\s chars-to-insert)))

            ;; Calculate `prefix-string' for nice `auto-fill' (using
            ;; overlays)
            (let ((prefix-string (make-string (- (point-max) (point-min)) ?\s))
                  (text-start (point)))
              ;; trim & handle color codes
              (let* ((text (weechat-> text
                                      (s-trim)
                                      (weechat-handle-color-codes)
                                      (propertize 'weechat-text t))))
                (insert (cond
                         (highlight
                          (propertize text 'face 'weechat-highlight-face))
                         ((eq line-type :irc/x-error)
                          (propertize text 'face 'weechat-error-face))
                         (t text))
                        "\n"))

              (when weechat-fill-text
                ;; Filling is slightly misleading here.  We use this
                ;; awesome text property called `wrap-prefix'.
                (let ((overlay (make-overlay text-start (point-max))))
                  (overlay-put overlay 'wrap-prefix
                               (propertize prefix-string 'face 'default)))))

            ;; Go to start of inserted line
            (goto-char (1- (point)))    ;skip newline
            (goto-char (point-at-bol))

            ;; Add general properties
            (weechat-line-add-properties nick date highlight invisible)

            ;; Important: Run the hook after everything else
            (save-restriction
              (run-hooks 'weechat-insert-modify-hook))))

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

        ;; Recenter window if there are more lines than fit in the
        ;; frame.  This is borrowed from rcirc.
        (weechat-recenter-bottom-maybe)

        (set-marker-insertion-type weechat-prompt-start-marker nil)
        (set-marker-insertion-type weechat-prompt-end-marker nil))

      ;; Truncate
      (weechat-truncate-buffer)

      ;; Drop undo information (borrowed from weechat)
      (when (not (s-blank? (weechat-get-input)))
        (buffer-disable-undo)
        (buffer-enable-undo)))))

(defun weechat-line-type (line-hdata)
  (let ((tags (cdr (assoc-string "tags_array" line-hdata))))
    (cond
     ((member "irc_join" tags) :irc/join)
     ((member "irc_action" tags) :irc/action)
     ((member "irc_part" tags) :irc/part)
     ((member "irc_quit" tags) :irc/quit)
     ((member "irc_mode" tags) :irc/mode)
     ((member "irc_nick" tags) :irc/nick)
     ((member "irc_topic" tags) :irc/topic)
     ((member "irc_numeric" tags) :irc/numeric)
     ((member "irc_notice" tags) :irc/notice)
     ((member "irc_privmsg" tags) :irc/privmsg)
     (:irc/unknown))))                     ;fallback

(defun weechat-buffer-type (&optional buffer-ptr)
  (let* ((buffer-ptr (or buffer-ptr weechat-buffer-ptr))
         (type (weechat->> buffer-ptr
                           (weechat-buffer-hash)
                           (gethash "local_variables")
                           (assoc-string "type")
                           (cdr)) ))
    (when (stringp type)
      (intern (format ":%s" type)))))

(defvar weechat-user-list)
(defun weechat--user-list-add (nick)
  (unless (s-blank? nick)
    (setq weechat-user-list (cons nick (delete nick weechat-user-list)))))
(defun weechat--user-list-remove (nick)
  (setq weechat-user-list (delete nick weechat-user-list)))

(defun weechat--get-nick-from-tag (line-hdata &optional nick-tag)
  "Get nick name from tags_array in LINE-HDATA.
If NICK-TAG is nil then \"nick_\" as prefix else use NICK-TAG."
  (setq nick-tag (or nick-tag "nick_"))
  (let ((tags-array (cdr (assoc-string "tags_array" line-hdata))))
    (when tags-array
      (s-chop-prefix
       nick-tag
       (cl-find-if (lambda (s) (s-prefix? nick-tag s)) tags-array)))))

(defun weechat--get-nick-from-line-data (line-hdata)
  "Get nick name from LINE-HDATA."
  (or
   (weechat--get-nick-from-tag line-hdata)
   (let* ((prefix (cdr (assoc-string "prefix" line-hdata)))
          ;; Try to strip the color and prefix from nick
          (nick-match (s-match "\x19\F[[:digit:]][[:digit:]]\\([^\x19]+\\)$" prefix)))
     (or (cadr nick-match) prefix ""))))

(defun weechat-print-line-data (line-data)
  (let* ((buffer-ptr (assoc-default "buffer" line-data))
         (buffer (weechat--emacs-buffer buffer-ptr)))
    (unless (weechat-buffer-hash buffer-ptr)
      (error "Received new line for '%s' but the buffer doesn't exist in local cache" buffer-ptr))
    (let ((prefix (assoc-default "prefix" line-data))
          (message (assoc-default "message" line-data))
          (date (assoc-default "date" line-data))
          (highlight (assoc-default "highlight" line-data nil 0))
          (line-type (weechat-line-type line-data))
          (invisible (not (= 1 (assoc-default "displayed" line-data nil 0))))
          (nick (weechat--get-nick-from-line-data line-data)))
      ;; Handle lines printed to weechat buffers that aren't in weechat-mode
      (when (boundp 'weechat-lines-received)
        (setq weechat-lines-received (+ weechat-lines-received 1)))
      (unless invisible
        (setq highlight (= 1 highlight))
        (when (bufferp (weechat--emacs-buffer buffer-ptr))
          (with-current-buffer buffer
            (when weechat-strip-formatting
              (setq prefix (weechat-strip-formatting prefix))
              (setq message (weechat-strip-formatting message)))

            ;; Nicklist handling.  To be replaced with real nicklist
            ;; updates when WeeChat starts sending nicklist deltas
            (if (or (and weechat-complete-order-nickname (eq line-type :irc/privmsg))
                    (eq line-type :irc/join))
                (weechat--user-list-add nick)
              (cl-case line-type
                (:irc/nick
                 (let ((from-nick (weechat--get-nick-from-tag line-data "irc_nick1_"))
                       (to-nick (weechat--get-nick-from-tag line-data "irc_nick2_")))
                   (when (and from-nick to-nick)
                     (weechat--user-list-remove from-nick)
                     (weechat--user-list-add to-nick))))
                ((:irc/part :irc/quit) (weechat--user-list-remove nick)))))

          ;; Print the line
          (cl-case line-type
            (:irc/action
             (let ((weechat-text-column 0))
               (weechat-print-line buffer-ptr
                                   :text (concat prefix message)
                                   :nick nick
                                   :line-type line-type
                                   :date date
                                   :highlight highlight)))
            (t
             (weechat-print-line buffer-ptr
                                 :prefix prefix
                                 :text message
                                 :nick nick
                                 :date date
                                 :line-type line-type
                                 :highlight highlight
                                 :invisible invisible))))

        (weechat-update-buffer-modified buffer-ptr line-data)

        ;; TODO: Debug highlight for monitored and un-monitored channels
        ;; (Maybe) notify the user
        (with-current-buffer (or (and (buffer-live-p buffer) buffer)
                                 (get-buffer weechat-relay-log-buffer-name)
                                 (current-buffer))
          (let* ((buftype (weechat-buffer-type buffer-ptr))
                 (highlight (cl-case buftype
                              (:private t) ;always highlight queries
                              (:server nil) ;never highlight server buffers
                              (t highlight)))
                 (type (cl-case buftype
                         (:private (unless (string=
                                            (weechat-get-local-var
                                             "nick"
                                             buffer-ptr)
                                            nick)
                                     :query))
                         (:channel :highlight))))
            (when (and (not weechat-inhibit-notifications)
                       highlight
                       type)
              (weechat-notify type
                              :sender nick
                              :text message
                              :date date
                              :buffer-ptr buffer-ptr)))

          (run-hook-with-args 'weechat-message-post-receive-functions buffer-ptr))))))

(defun weechat-add-initial-lines (response)
  (let* ((lines-hdata (car response))
         (hdata-values
          (weechat--hdata-values lines-hdata)))
    (when hdata-values
      (let ((buf-ptr (weechat->
                      hdata-values
                      (car)
                      (weechat--hdata-value-pointer-path)
                      (car))))
        ;; Need to get buffer-ptr from hdata pointer list
        (with-current-buffer (weechat--emacs-buffer buf-ptr)
          (save-excursion
            (let ((weechat-inhibit-notifications t))
              (dolist (line-hdata (weechat--hdata-values lines-hdata))
                (weechat-print-line-data (weechat--hdata-value-alist line-hdata))))
            (weechat-recenter-bottom-maybe nil 'force)))))))

(defvar weechat-initial-lines-buffer-properties
  '("message" "highlight" "prefix" "date" "buffer" "displayed" "tags_array"))

(defun weechat-request-initial-lines (buffer-ptr)
  (weechat-relay-log
   (format "Requesting %i lines for buffer %s"
           weechat-initial-lines
           buffer-ptr))
  (weechat-relay-send-command
   (format "hdata buffer:%s/lines/last_line(-%i)/data %s"
           buffer-ptr
           weechat-initial-lines
           (s-join "," weechat-initial-lines-buffer-properties))
   #'weechat-add-initial-lines))

(defvar weechat-send-input-last-target nil
  "Internal var used to track last message's target.")
(defun weechat-send-input (target input)
  (if (not (weechat-connected-p))
      (error "Not connected")
    (when (and weechat-sync-active-buffer
               (not (s-equals? weechat-send-input-last-target
                               target)))
      ;; HACK: Switch active buffer on the relay server
      (weechat-relay-send-command
       (format "input %s /buffer %s" target (weechat-buffer-name target))))
    (weechat-relay-send-command
     (format "input %s %s" target input))
    (setq weechat-send-input-last-target target)))

(defun weechat-get-input ()
  (s-trim-right
   (buffer-substring-no-properties
    weechat-prompt-end-marker
    (point-max))))

(defun weechat-replace-input (replacement &optional not-move-eol)
  "Replace input line with REPLACEMENT.
If NOT-MOVE-EOL is non-nil the point is not changed else it is moved to
the end of line."
  (save-excursion
    (delete-region weechat-prompt-end-marker (point-max))
    (goto-char weechat-prompt-end-marker)
    (insert (or replacement "")))
  (unless not-move-eol
    (move-end-of-line 1)))

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

(defun weechat-pipe-input (text)
  (cl-reduce (lambda (s f)
               (when (stringp s)
                 (funcall f s)))
             weechat-message-filter-functions
             :initial-value text))

(defun weechat-return ()
  "Return key action.
If point is in input field send message.  If the point is in a chat line
copy the message.  Only the message text is copied unless the prefix argument
is given (\\[universal-argument])."
  (interactive)
  (cond
   ;; Submit
   ((>= (point) weechat-prompt-end-marker)
    (let ((input (weechat-get-input)))
      (unless (s-blank? input)
        ;; Split multiple lines and send one-by-one
        (cl-dolist (l (split-string input "\n"))
          ;; Pipe the input through the message filter system
          (let ((piped-input (weechat-pipe-input l)))
            (when (stringp piped-input)
              (if (and (not (s-equals? piped-input l))
                       weechat-message-filter-require-double-ret)
                  ;; Either filtered text is unchanged or we don't want double-ret anyway
                  (progn
                    ;; Replace text in-place and display a message
                    (weechat-replace-input piped-input)
                    (message "Input text was filtered..."))
                ;; No change, just send it
                (weechat-send-input weechat-buffer-ptr piped-input)
                (weechat-replace-input "")))))
        (weechat-input-ring-insert input))))

   ;; Copy current line to input line
   ((< (point) weechat-prompt-start-marker)
    (when (or (s-blank? (weechat-get-input))
              weechat-return-always-replace-input)
      (weechat-replace-input
       (buffer-substring-no-properties
        (if current-prefix-arg
            (point-at-bol)
          (+ (point-at-bol) weechat-text-column))
        (point-at-eol))))
    (goto-char (point-max)))))

(defun weechat-self-insert-command (n)
  "Like `self-insert-commands' with automatic cursor movement."
  (interactive "p")
  (when (and weechat-auto-move-cursor-to-prompt
             weechat-read-only
             (< (point) weechat-prompt-start-marker))
    (goto-char (point-max)))
  (self-insert-command n))

;;; Make `weechat-self-insert-command' work with some modes
;;; Borrowed from org.el
(put 'weechat-self-insert-command 'delete-selection t)
(put 'weechat-self-insert-command 'flyspell-delayed t)
(put 'weechat-self-insert-command 'pabbrev-expand-after-command t)

(defun weechat-bol (&optional arg)
  "Go to the beginning of line, then skip past the prompt, if any.
If prefix argument is given (\\[universal-argument]) the prompt is not skipped."
  ;; basically copied from `comint-bol'.
  (interactive "P")
  (cond
   (arg (forward-line 0))
   ((> (point) weechat-prompt-start-marker) (goto-char weechat-prompt-end-marker))
   (t (beginning-of-line))))

(defvar weechat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'weechat-return)
    (define-key map (kbd "M-p") 'weechat-previous-input)
    (define-key map (kbd "M-n") 'weechat-next-input)
    (define-key map (kbd "C-c C-r") 'weechat-reload-buffer)
    (define-key map (kbd "C-c C-g") 'weechat-get-more-lines)
    (define-key map (kbd "TAB") 'completion-at-point)
    (define-key map (kbd "C-a") 'weechat-bol)
    (define-key map (kbd "C-c n l") 'weechat-narrow-to-line)
    (define-key map (kbd "C-c C-b") 'weechat-switch-buffer)
    (define-key map (kbd "C-c C-m") 'weechat-monitor-buffer)
    map)
  "Keymap for weechat mode.")

(substitute-key-definition
 'self-insert-command
 'weechat-self-insert-command
 weechat-mode-map
 global-map)

(easy-menu-define weechat-mode-menu weechat-mode-map
  "Weechat menu"
  '("WeeChat"
    ["Previous Input" weechat-previous-input t]
    ["Next Input" weechat-next-input t]
    "-"
    ["Narrow To Line" weechat-narrow-to-line
     :active (< (point) weechat-prompt-start-marker)]
    "-"
    ["Reload Buffer" weechat-reload-buffer t]
    ["Get More Lines" weechat-get-more-lines t]
    ["Close Buffer" kill-buffer t]
    ["Switch Buffer" weechat-switch-buffer t]
    ["Monitor Buffer" weechat-monitor-buffer t]
    ["Connect" weechat-connect
     :visible (not (weechat-connected-p))]
    ["Disconnect" weechat-disconnect
     :visible (weechat-connected-p)]))

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

  ;; Hack to restore prompt location
  (let ((prompt-start (when (boundp 'weechat-prompt-start-marker)
                        weechat-prompt-start-marker))
        (prompt-end (when (boundp 'weechat-prompt-end-marker)
                      weechat-prompt-end-marker)))

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
    (set (make-local-variable 'weechat-lines-received) 0)

    ;; Start with empty user list
    (set (make-local-variable 'weechat-user-list) nil)

    ;; Setup prompt
    (make-local-variable 'weechat-local-prompt)
    (set (make-local-variable 'weechat-prompt-start-marker)
         (or prompt-start(point-max-marker)))
    (set (make-local-variable 'weechat-prompt-end-marker)
         (or prompt-end(point-max-marker)))
    (weechat-update-prompt)

    ;; Initialize input-ring
    (set (make-local-variable 'weechat-input-ring) (make-ring weechat-input-ring-size))

    ;; Don't auto-add newlines on next-line
    (set (make-local-variable 'next-line-add-newlines) nil)
    ;; Fix scrolling
    (set (make-local-variable 'scroll-conservatively) 1000)
    (set (make-local-variable 'scroll-margin) 0)

    ;; Initialize buffer
    (weechat-request-initial-lines buffer-ptr)

    ;; Set Header
    (weechat-update-header-line-buffer (current-buffer))

    ;; Hooks
    (run-mode-hooks 'weechat-mode-hook)))

(defun weechat--try-ido (&rest args)
  "Complete with ido if available and `completing-read' otherwise."
  (apply (or (and (featurep 'ido)
                  (symbol-function 'ido-completing-read))
             #'completing-read)
         args))

(defun weechat--try-ivy (&rest args)
  "Complete with ivy if available and `completing-read' otherwise."
  (apply (or (and (featurep 'ivy)
                  (symbol-function 'ivy-read))
             #'completing-read)
         args))

(defun weechat--read-channel-name (&optional only-monitored)
  "Read channel name from minibuffer in combination with `interactive'."
  (weechat--find-buffer
   (funcall weechat-completing-read-function
            "Channel Name: "
            (weechat-channel-names only-monitored 'sort))))

(defun weechat-monitor-buffer (buffer-ptr &optional show-buffer)
  "Start monitoring BUFFER-PTR.
If SHOW-BUFFER is non-nil `switch-to-buffer' after monitoring it."
  (interactive (list
                (when (weechat-connected-p)
                  (weechat--read-channel-name))
                t))
  (if (not (weechat-connected-p))
      (error "Can't monitor buffer, not connected.")
    (save-excursion
      (let* ((buffer-hash (weechat-buffer-hash buffer-ptr))
             (name (weechat-buffer-name buffer-ptr)))
        (unless (hash-table-p buffer-hash)
          (error "Couldn't find buffer %s on relay server" buffer-ptr))

        ;; Notify the user via `weechat-monitor-buffer-function'
        (when weechat-monitor-buffer-function
          (cond
           ((eq 'message weechat-monitor-buffer-function)
            (message "Monitoring new Buffer: %s" name))
           ((functionp weechat-monitor-buffer-function)
            (with-demoted-errors
              (funcall weechat-monitor-buffer-function buffer-ptr)))))

        (with-current-buffer (get-buffer-create name)
          (let ((inhibit-read-only t))
            (when (weechat-buffer-p)
              (delete-region (point-min) weechat-prompt-start-marker)))
          (weechat-mode (get-buffer-process weechat-relay-buffer-name)
                        buffer-ptr
                        buffer-hash)
          (when show-buffer
            (switch-to-buffer (current-buffer))))))))

(defun weechat-switch-buffer (buffer-ptr)
  "Like `switch-buffer' but limited to WeeChat buffers.

BUFFER-PTR is a string containing a pointer to the buffer to switch to.

Will monitor channels if necessary.  Will list remotely available buffers if
called with prefix (\\[universal-argument]), otherwise only monitored buffers."
  (interactive (list (weechat--read-channel-name (not current-prefix-arg))))
  (let ((buffer (weechat--emacs-buffer buffer-ptr)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer)
      (weechat-monitor-buffer buffer-ptr 'show))))

(defun weechat-reload-buffer (&optional buffer line-count)
  (interactive (list (current-buffer)
                     current-prefix-arg))
  (weechat-load-buffer (current-buffer) buffer line-count))

(defun weechat-get-more-lines (&optional buffer line-count)
  (interactive (list (current-buffer)
                     current-prefix-arg))
  (weechat-load-buffer (current-buffer)
                       buffer
                       (max (+ weechat-lines-received
                               (or line-count weechat-more-lines-amount))
                            0)))

(defun weechat-load-buffer (current-buffer &optional buffer line-count)
  (if (not (weechat-connected-p))
      (error "Can't reload buffer. Not connected.")
   (with-current-buffer (or buffer (current-buffer))
     (weechat-relay-log
      (format "Re-monitoring buffer %s" (buffer-name buffer)))
     (let ((weechat-initial-lines (or line-count
                                      weechat-initial-lines)))
       (weechat-monitor-buffer weechat-buffer-ptr)))))

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
  (let* ((available-channels (weechat-channel-names))
         (chans (cond
                 ((listp weechat-auto-monitor-buffers)
                  weechat-auto-monitor-buffers)
                 ((stringp weechat-auto-monitor-buffers)
                  (cl-remove-if-not (lambda (b)
                                      (s-matches? weechat-auto-monitor-buffers b))
                                    available-channels))
                 (t (progn
                      (weechat-message "Monitoring all available WeeChat buffers.  Be patient...")
                      available-channels)))))
    ;; Either iterate ALL available channels (for `t') or iterate
    ;; channels user wants to monitor
    (dolist (channel chans)
      ;; Check if one of the available channels partially matches the
      ;; channel we want to monitor
      (let* ((channel-name (cl-some
                            (lambda (ac)
                              ;; NOTE: We use `s-suffix?' as we need
                              ;; to ignore the server-prefix in
                              ;; `channel'. `s-contains?' causes
                              ;; errors if two channels share the same
                              ;; prefix.
                              (when (s-suffix? channel ac) ac))
                            available-channels))
             (buffer-ptr (weechat--find-buffer channel-name)))
        ;; Only auto-connect if it there isn't already a buffer monitoring the channel
        (if buffer-ptr
            (unless (weechat--emacs-buffer buffer-ptr)
              (weechat-relay-log (format "Auto-monitoring buffer %S" channel-name) :info)
              (weechat-monitor-buffer buffer-ptr nil))
          (weechat-warn "Couldn't monitor channel '%s'.  Not found." channel))))))


(add-hook 'weechat-connect-hook 'weechat-auto-monitor 'append)

(defun weechat--handle-buffer-line-added (response)
  (let* ((hdata (car response))
         (line-data (weechat--hdata-value-alist (car (weechat--hdata-values hdata)))))
    (weechat-print-line-data line-data)))

(weechat-relay-add-id-callback "_buffer_line_added" #'weechat--handle-buffer-line-added nil 'force)

(defun weechat-join (channel)
  "Join CHANNEL."
  (let ((full-name (cl-some (lambda (x) (when (s-contains? channel x) x))
                            (weechat-channel-names))))
    (if full-name
        (weechat-monitor-buffer (weechat--find-buffer full-name) 'show)
      (weechat-send-input weechat-buffer-ptr (concat "/join " channel)))))

;;; This should probably be in some util file:

(defun weechat--send-cmd (cmd &rest options)
  "Send CMD with OPTIONS to WeeChat."
  (weechat-send-input
   weechat-buffer-ptr
   (concat cmd " " (when options
                     (cl-reduce (lambda (l r)
                                  (concat l " " r))
                                options)))))

(defcustom weechat-nick-operations
  '(("DeOp" .  (weechat--send-cmd "/deop" nick))
    ("Kick" . (weechat--send-cmd "/kick" nick
                                 (read-from-minibuffer
                                  (concat "Kick " nick ", reason: "))))
    ("Query" . (weechat--send-cmd "/query" nick))
    ("Whois" . (weechat--send-cmd "/whois" nick))
    ("Op" . (weechat--send-cmd "/op" nick))
    ("Voice" . (weechat--send-cmd "/voice" nick)))
  "An alist of possible nickname actions.
The format is (\"Action\" . SEXP) wher SEXP is evaluated with `nick' bound."
  :group 'weechat
  :type '(repeat (const (string :tag "Action")
                        sexp)))

(defun weechat-nick-action (nick)
  "Ask user for action on NICK and `eval' it."
  (let* ((completion-ignore-case t)
         (action (completing-read (concat "What action to take on '" nick "'? ")
                                  weechat-nick-operations))
         (code `(let ((nick ,nick))
                  ,(cdr (assoc-string action weechat-nick-operations)))))
    (when code
      (eval code))))

(provide 'weechat)

;;; weechat.el ends here
