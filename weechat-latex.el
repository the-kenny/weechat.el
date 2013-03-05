;;; weechat-latex --- Add LateX preview -*- lexical-binding: t -*-

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
;; The LaTeX preview is based on `org-mode's `org-format-latex'

;;; Code:

(require 'weechat)
(require 'org)

(defgroup weechat-latex nil
  "WeeChat LaTeX preview."
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-latex"
  :group 'weechat)

(defcustom weechat-latex-temp-file-prefix "weechat-latex"
  "Prefix for temporary files."
  :type 'string
  :group 'weechat-latex)

(defcustom weechat-latex-temp-directory-prefix "weechat-latex"
  "Prefix for temporary directory."
  :type 'string
  :group 'weechat-latex)

(defcustom weechat-latex-image-program org-latex-create-formula-image-program
  "Program to convert LaTeX fragments.
See `org-latex-create-formula-image-program'"
  :type '(choice
	  (const :tag "dvipng" dvipng)
	  (const :tag "imagemagick" imagemagick))
  :group 'weechat-latex)

(defvar weechat-latex-temp-dir nil
  "The temporary directory used for preview images.")

(defun weechat-latex--create-preview (at)
  "Wrapper for `org-format-latex'.
The parameter AT should be nil or in (TYPE . POINT) format.  With TYPE being a
string showing the matched LaTeX statement (e.g., ``$'') and POINT being the
POINT to replace.  If AT is nil replace statements everywhere."
  (org-format-latex weechat-latex-temp-file-prefix
                    weechat-latex-temp-dir
                    'overlays
                    "Creating images...%s"
                    at 'forbuffer
                    weechat-latex-image-program))

(defun weechat-latex--set-temp-dir ()
  "Set `weechat-latex-temp-dir' unless it is already set."
  (unless weechat-latex-temp-dir
    (setq weechat-latex-temp-dir
          (make-temp-file weechat-latex-temp-directory-prefix
                          'directory))))

(defun weechat-latex-preview ()
  "Preview LaTeX fragments."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (weechat-latex--set-temp-dir)
      (org-remove-latex-fragment-image-overlays)
      (weechat-latex--create-preview nil))))

(defun weechat-latex-preview-region (beg end)
  "Preview LaTeX fragments in region."
  (interactive "r")
  (let* ((math-regex (assoc "$" org-latex-regexps))
         (regex (nth 1 math-regex))
         (n (nth 2 math-regex))
         matches)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regex end t)
        (setq matches (cons (cons "$" (match-beginning n)) matches)))
      (let ((inhibit-read-only t))
        (weechat-latex--set-temp-dir)
        (dolist (i matches)
          (weechat-latex--create-preview i))))))

(defvar weechat-prompt-start-marker) ;; See weechat.el
(defun weechat-latex-preview-line ()
  "Preview LaTeX fragments in line."
  (interactive)
  (weechat-latex-preview-region (point-at-bol)
                                (min (point-at-eol)
                                     weechat-prompt-start-marker)))

(defun weechat-latex-remove ()
  "Remove LaTeX preview images."
  (interactive)
  (let ((inhibit-read-only t))
    (org-remove-latex-fragment-image-overlays)))

(defun weechat-latex-is-active? ()
  "Are LaTeX Previews currently displayed?"
  org-latex-fragment-image-overlays)

(defun weechat-latex-toggle ()
  "Toggle display of LaTeX preview."
  (interactive)
  (if (weechat-latex-is-active?)
      (weechat-latex-remove)
    (weechat-latex-preview)))

;;; auto mode

(defun weechat-latex--do-auto-mode ()
  "Hook for auto LaTeX preview."
  (weechat-latex-preview-region (point-min) (point-max)))

(defun weechat-latex-is-auto-mode-active? ()
  "Is auto LaTeX preview active?"
  (memq #'weechat-latex--do-auto-mode weechat-insert-modify-hook))

(defcustom weechat-latex-auto-mode-line-string " LaTeX-Preview"
  "String displayed in mode line when `weechat-latex-auto-mode' is active."
  :type 'string
  :group 'weechat-latex)

(defcustom weechat-latex-auto-mode-preview-all t
  "Show preview for existing LaTeX fragmetns if auto mode is activated?"
  :type 'boolean
  :group 'weechat-latex)

(define-minor-mode weechat-latex-auto-mode
  "Automatically display LaTeX preview."
  :lighter weechat-latex-auto-mode-line-string
  :group 'weechat-latex
  (if weechat-latex-auto-mode
      (progn
        (when weechat-latex-auto-mode-preview-all
          (weechat-latex-preview))
        (add-hook 'weechat-insert-modify-hook #'weechat-latex--do-auto-mode))
    (remove-hook 'weechat-insert-modify-hook #'weechat-latex--do-auto-mode)))

;;; module

(easy-menu-add-item weechat-mode-menu nil
                    ["LaTeX Preview" weechat-latex-toggle
                     :style toggle
                     :selected (weechat-latex-is-active?)
                     :help "If selected show LaTeX preview for existing buffer."]
                    "Toggle Hidden Lines")

(easy-menu-add-item weechat-mode-menu nil
                    ["LaTeX Auto Preview" weechat-latex-auto-mode
                     :style toggle
                     :selected (weechat-latex-is-auto-mode-active?)
                     :help "If selected automatically show LaTeX preview for new messages."]
                    "Toggle Hidden Lines")

(defun weechat-latex-unload-function ()
  "Cleanup WeeChat LaTex module."
  (weechat-latex-auto-mode -1)
  (weechat-latex-remove)
  (easy-menu-remove-item weechat-mode-menu nil "LaTeX Preview")
  (easy-menu-remove-item weechat-mode-menu nil "LaTeX Auto Preview"))

(provide 'weechat-latex)

;;; weechat-latex.el ends here
