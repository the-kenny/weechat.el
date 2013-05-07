;;; weechat-corrector.el --- Fix your messages using s/foo/bar/ syntax

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
;; This module implements support to fix your own messages via the
;; s/foo/bar/ syntax.


;;; Code:

(require 'weechat)
(require 's)

(defgroup weechat-corrector nil
  "This module implements support to fix your own messages via the s/foo/bar/ syntax."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-corrector"
  :group 'weechat)

(defcustom weechat-corrector-search-limit 5
  "How many previous lines to check for corrections."
  :type 'integer
  :group 'weechat-corrector)

(defcustom weechat-corrector-replace-limit 1
  "Limit to N replacements."
  :type 'integer
  :group 'weechat-corrector)

(defcustom weechat-corrector-correct-other nil
  "Whether to apply corrections by other people.

Warning: Setting this to non-nil MIGHT be a security problem as untrusted
regular expression will be interpreted by `re-search-forward'."
  :type 'boolean
  :group 'weechat-corrector)

(defcustom weechat-corrector-support-plain-parentheses nil
  "If non-nil, s/a(.)c/\1/ will replace 'abc' with 'b'.

If nil, parentheses must be quotedL s/a\(.\)c/\1/."
  :type 'boolean
  :group 'weechat-corrector)

(defface weechat-corrector-corrected-face '((t :inherit default))
  "Face used to highlight corrected text."
  :group 'weechat-corrector)

(defun weechat-corrector-quote-parentheses (re)
  (if weechat-corrector-support-plain-parentheses
      (weechat->>
       re
       (s-replace "(" "\\(")
       (s-replace ")" "\\)"))
    re))

(defvar weechat-corrector-regex "s/\\(.+\\)/\\(.*\\)/")
(defun weechat-corrector-apply ()
  (let ((nick (weechat-line-nick))
        (line (weechat-line-text)))
    (when (and (or weechat-corrector-correct-other
                   (string= (weechat-line-nick)
                            (weechat-get-local-var "nick")))
               line
               (stringp line))
      (let* ((text-start (weechat-line-text-start))
             (match (s-match weechat-corrector-regex line)))
        (when (>= (length match) 3)
          (let ((re (weechat-corrector-quote-parentheses
                     (cadr match)))
                (rp (caddr match)))
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-at-bol))
                (let ((count 0))
                  (dotimes (i weechat-corrector-search-limit)
                    (when (< count weechat-corrector-replace-limit)
                      (save-restriction
                        (let ((line-move-visual nil))
                          (forward-line -1))
                        (weechat-narrow-to-line)
                        (goto-char (weechat-line-text-start))
                        (when (and (string= nick (weechat-line-nick))
                                   (re-search-forward re nil t))
                          (replace-match rp)
                          ;; Add `weechat-corrector-corrected-face'
                          (add-text-properties (match-beginning 0) (match-end 0)
                                               '(face weechat-corrector-corrected-face))
                          (setq count (1+ count)))))))))))))))

(add-hook 'weechat-insert-modify-hook 'weechat-corrector-apply)

(provide 'weechat-corrector)

;;; weechat-corrector.el ends here
