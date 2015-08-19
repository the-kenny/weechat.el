;;; weechat-complete --- completions for weechat.el. ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Parts copied from `erc-pcomplete.el' are
;; Copyright (C) 2002-2004, 2006-2013 Free Software Foundation, Inc.

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;;         Moritz Ulrich <moritz@tarn-vedra.de>
;;         Aristid Breitkreuz <aristidb@gmail.com>
;;         Martin Yrjölä <martin.yrjola@gmail.com>
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

;; Completions for weechat.el.  Parts of the code are copied from
;; `erc-pcomplete.el'.

;;; Code:

(require 'weechat)
(require 'pcomplete)
(require 's)

(defvar weechat-user-list)
(defvar weechat-prompt-end-marker)

(defcustom weechat-complete-nick-prefix-and-postfix-alist nil
  "Alist of nick prefixes and postfixes to use for certain
buffers Each element looks like (BUFFER-NAME-REGEXP
. (NICK-PREFIX . NICK-POSTFIX). If BUFFER-NAME-REGEXP matches the
current buffer, then NICK-PREFIX is pushed to the beginning of
each nick in `weechat-user-list' and NICK-POSTFIX is appended to
them. One common use case is that a messaging service provides an
IRC server (e.g. Flowdock and Gitter), but the mentions have to
be prefixed with '@' to be highlighted in the other clients."
  :type '(alist :key-type regexp :value-type (cons string string))
  :group 'weechat)

(defcustom weechat-complete-nick-postfix ":"
  "Postfix to nick completions at the beginning of the prompt.
Can be overriden by `weechat-complete-nick-prefix-and-postfix-alist'"
  :type 'string
  :group 'weechat)

(defcustom weechat-complete-nick-ignore-self t
  "Wether to ignore yourself when completing at the begginning of
the input line

Make sure to rebuild each buffer after changing this variable."
  :type 'boolean
  :group 'weechat)

(defun weechat-pcompletions-at-point ()
  "WeeChat completion data from pcomplete.
for use on `completion-at-point-function'.

Copied from `erc-pcompletions-at-point'."
  (when (and (eq major-mode 'weechat-mode)
             (>= (point) weechat-prompt-end-marker))
    (or (let ((pcomplete-default-completion-function #'ignore))
          (pcomplete-completions-at-point))
        (let ((c (pcomplete-completions-at-point)))
          (if c (nconc c '(:exclusive no)))))))

(defun pcomplete-weechat-setup ()
  "Setup pcomplete for `weechat-mode'."
  (set (make-local-variable 'pcomplete-ignore-case) t)
  (set (make-local-variable 'pcomplete-use-paring) nil)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       #'pcomplete-weechat-parse-arguments)
  (set (make-local-variable 'pcomplete-command-completion-function)
       #'pcomplete/weechat/complete-command)
  (set (make-local-variable 'pcomplete-command-name-function)
       #'pcomplete-weechat-command-name)
  (set (make-local-variable 'pcomplete-default-completion-function)
       (lambda () (pcomplete-here (pcomplete-weechat-nicks)))))

(defun pcomplete-weechat-parse-arguments ()
  "Return a list of parsed whitespace-separated arguments.
These are the words from the beginning of the line after the prompt
up to where point is right now.

Copied from `pcomplete-erc-parse-arguments'."
  (let* ((start weechat-prompt-end-marker)
         (end (point))
         args beginnings)
    (save-excursion
      (when (< (skip-chars-backward " \t\n" start) 0)
        (setq args '("")
              beginnings (list end)))
      (setq end (point))
      (while (< (skip-chars-backward "^ \t\n" start) 0)
        (setq beginnings (cons (point) beginnings)
              args (cons (buffer-substring-no-properties
                          (point) end)
                         args))
        (skip-chars-backward " \t\n" start)
        (setq end (point))))
    (cons args beginnings)))

(defun pcomplete-weechat-command-name ()
  "Return the command name of the first argument.
Copied from `pcomplete-erc-command-name'."
  (if (eq (aref (pcomplete-arg 'first) 0) ?/)
      (upcase (substring (pcomplete-arg 'first) 1))
    "SAY"))

(defun pcomplete/weechat/complete-command ()
  "Complete the initial command argument."
  (pcomplete-here
   (append
    (pcomplete-weechat-commands)
    (pcomplete-weechat-nicks weechat-complete-nick-postfix weechat-complete-nick-ignore-self))))

(defun pcomplete/weechat-mode/WHOIS ()
  (pcomplete-here (pcomplete-weechat-all-nicks)))

(defun pcomplete/weechat-mode/QUERY ()
  (pcomplete-here (pcomplete-weechat-all-nicks)))

(defun pcomplete/weechat-mode/SAY ()
  (while (pcomplete-here (pcomplete-weechat-nicks))))

(defun pcomplete-weechat-commands ()
  "Return a list of user commands."
  '("/NICK" "/JOIN" "/PART" "/WHOIS" "/QUERY")) ;; TODO

(defun pcomplete-weechat-nicks (&optional postfix ignore-self)
  "Return a list of nicks in the current channel.
POSTFIX is an optional string to append to the nickname.
If IGNORE-SELF is non-nil the users nick is ignored."
  (let* ((users weechat-user-list)
         (prefix-and-postfix
          (weechat-complete-nick-prefix-and-postfix-for-current-buffer))
         (nick-prefix (car prefix-and-postfix))
         (nick-postfix (if prefix-and-postfix
                      (cdr prefix-and-postfix)
                    postfix)))
    (when ignore-self
      (setq users (delete (weechat-get-local-var "nick") users)))
    (mapcar (lambda (nick)
              (concat
               nick-prefix
               nick
               nick-postfix)) users)))

(defun weechat-complete-nick-prefix-and-postfix-for-current-buffer ()
  "Returns the nick-prefix-and-postfix for the current buffer.
The format is (nick-prefix . nick-postfix)."
  (let ((found-nick-prefix-and-postfix nil))
    (mapc (lambda (element)
            (let ((buffer-name-regexp (car element))
                  (nick-prefix-and-postfix (cdr element)))
              (when (string-match buffer-name-regexp (buffer-name))
                (setq found-nick-prefix-and-postfix nick-prefix-and-postfix))))
          weechat-complete-nick-prefix-and-postfix-alist)
    found-nick-prefix-and-postfix))

(defun pcomplete-weechat-all-nicks ()
  "Return nick list of all weechat buffers."
  (let (result)
    (weechat-do-buffers
     (setq result (cl-union weechat-user-list result :test #'s-equals?)))
    result))

(weechat-do-buffers (pcomplete-weechat-setup))
(add-hook 'weechat-mode-hook #'pcomplete-weechat-setup)
(add-hook 'completion-at-point-functions #'weechat-pcompletions-at-point)

(provide 'weechat-complete)

;;; weechat-complete.el ends here
