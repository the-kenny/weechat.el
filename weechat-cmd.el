;;; weechat-cmd.el --- Define local weechat commands in elisp

;; Copyright (C) 2013 Moritz Ulrich <moritz@tarn-vedra.de>

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
;; 
;; This modules allows the user to write Emacs Lisp functions which
;; are callable in weechat buffers via normal irc /command syntax.
;; 
;; Function names must start with `weechat-cmd-function-prefix' and
;; take one argument, the input of the user. 
;;
;; They might return either a string or nil. In case of string the
;; value will be sent to the server. If the return value is nil, the
;; command will be discarded. This is useful if you want to execute
;; local code but not send anything to the server.

;;; Code:

(require 'weechat)
(require 's)
(require 'cl-lib)

(defconst weechat-cmd-function-prefix "weechat-command-")

(defcustom weechat-cmd-prefix "/"
  "Prefix used for cmd commands."
  :type 'string
  :group 'weechat-cmd)

(defun weechat-cmd-find-cmds ()
  (let (ret)
    (mapatoms
     (lambda (x)
       (when (s-prefix? weechat-cmd-function-prefix (symbol-name x))
         (setq ret (cons x ret)))))
    ret))

(defun weechat-cmd-apply (input)
  (if (s-prefix? weechat-cmd-prefix input)
      (let* ((command (car
                       (s-split-words
                        (s-chop-prefix
                         weechat-cmd-prefix
                         input))))
             (sym (intern-soft (concat weechat-cmd-function-prefix
                                       command)))
             (fn (when (fboundp sym)
                   (symbol-function sym))))
        (if (functionp fn)
            (funcall fn input)
          input))
    input))

(add-hook 'weechat-message-filter-functions #'weechat-cmd-apply)

(provide 'weechat-cmd)

;;; weechat-cmd.el ends here
