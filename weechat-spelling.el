;;; weechat-spelling.el --- FlySpell support for WeeChat. -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;;         Moritz Ulrich <moritz@tarn-vedra.de>
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

;;; Code:

(require 'weechat)
(require 'flyspell)
(require 's)

(defgroup weechat-spelling nil
  "FlySpell support for WeeChat."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-spelling"
  :group 'weechat)

(defcustom weechat-spelling-dictionaries nil
  "An alist mapping buffer names to dictionaries.
The format of each entry is (CHANNEL . DICTIONARY).  Where CHANNEL is a regexp
matching the buffer name (\"Server.Channel\") and DICTIONARY is the name of an
`ispell' dictionary.

See `ispell-valid-dictionary-list' for a list of valid dictionaries."
  :type '(choice
          (const :tag "Default dictionary" nil)
          (repeat (cons (string :tag "Server.Channel Regex")
                        (string :tag "Dictionary"))))
  :group 'weechat-spelling)

(defun weechat-spelling-init (&optional buffer)
  "Initialize spelling in BUFFER or `current-buffer'."
  (with-current-buffer (or buffer (current-buffer))
    (dolist (entry weechat-spelling-dictionaries)
      (when (s-matches? (car entry) (buffer-name))
        (setq ispell-local-dictionary (cdr entry))))
    (setq flyspell-generic-check-word-predicate #'weechat-mode-flyspell-verify)
    (flyspell-mode 1)))

(defvar weechat-prompt-end-marker) ;; See weechat.el
(defvar weechat-user-list) ;; See weechat.el

(defun weechat-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' in `weechat-mode'."
  (not (or
        ;; Spell-check only input line
        (< (point) weechat-prompt-end-marker)
        (let ((word-data (flyspell-get-word)))
          (or
           ;; Don't spell-check nick names
           (member (car word-data) weechat-user-list)
           ;; Don't spell-check words starting with a /
           (eq (char-before (cadr word-data)) ?/))))))

(put 'weechat-mode
     'flyspell-mode-predicate
     #'weechat-mode-flyspell-verify)

(weechat-do-buffers (weechat-spelling-init))
(add-hook 'weechat-mode-hook #'weechat-spelling-init)

(defun weechat-spelling-unload-function ()
  (weechat-do-buffers (flyspell-mode -1))
  nil)

(provide 'weechat-spelling)

;;; weechat-spelling.el ends here
