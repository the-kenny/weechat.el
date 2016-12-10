;;; weechat-color --- Color handling for WeeChat ;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Moritz Ulrich, Rüdiger Sonderfeld

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

;; WeeChat comes with its own (ridiculously complicated) set of color codes.
;; See URL `http://www.weechat.org/files/doc/devel/weechat_dev.en.html#color_codes_in_strings'

;;; Code:

(require 'weechat-core)
(require 'rx)
(require 's)

(defgroup weechat-faces nil
  "WeeChat Faces and Color settings"
  :link '(url-link "https://github.com/the-kenny/weechat.el")
  :prefix "weechat-"
  :group 'weechat)

(defface weechat-nick-self-face '((t :weight bold :foreground "brown"))
  "Face for your own nick."
  :group 'weechat-faces)

(defface weechat-time-face '((t :inherit default))
  "Weechat face used for timestamps."
  :group 'weechat-faces)

(defface weechat-prompt-face '((t :inherit minibuffer-prompt))
  "Weechat face used for the prompt."
  :group 'weechat-faces)

(defface weechat-highlight-face
  '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
    (((class grayscale) (background dark)) :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "Purple")
    (((class color) (min-colors 88) (background dark))  :foreground "Cyan1")
    (((class color) (min-colors 16) (background light)) :foreground "Purple")
    (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
    (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
    (t :weight bold)) ;; Stolen from rcirc.el!
  "Weechat face for highlighted lines."
  :group 'weechat-faces)

(defface weechat-error-face '((t :inherit error))
  "Weechat face used to display errors."
  :group 'weechat-faces)

(defcustom weechat-strip-formatting nil
  "Remove every kind of formatting or color from messages.
This will look very bland!"
  :type 'boolean
  :group 'weechat-faces)

(defvar weechat-formatting-regex
  (let* ((attr `(in "*!/_|"))   ;NOTE:  is not documented
         (std  `(= 2 digit))
         (astd `(seq ,attr (= 2 digit)))
         (ext  `(seq "@" (= 5 digit)))
         (aext `(seq "@" ,attr (= 5 digit))))
    (rx-form
     `(or (seq ""
               (or ,std
                   ,ext
                   (seq "F" (or ,std ,astd ,ext ,aext))
                   (seq "B" (or ,std ,ext))
                   (seq "*" (or ,std
                                ,astd
                                ,ext
                                ,aext
                                (seq (or ,std ,astd ,ext ,aext)
                                     ","
                                     (or ,std ,astd ,ext ,aext))))
                   (seq "b" (in "FDB_-#il"))
                   ""))
          (seq "" ,attr)
          (seq "" ,attr)
          ""))))

(defun weechat-strip-formatting (string)
  "Strip weechat color codes from STRING."
  (if (stringp string)
      (replace-regexp-in-string weechat-formatting-regex "" string)
    string))

(defcustom weechat-color-list '(unspecified "black" "dark gray" "dark red" "red"
                                            "dark green" "light green" "brown"
                                            "yellow" "dark blue" "light blue"
                                            "dark magenta" "magenta" "dark cyan"
                                            "light cyan" "gray" "white")
  "Mapping of Weechat colors.

Do NOT remove or add new elements to the list.  Only change the values.
See URL `http://www.weechat.org/files/doc/devel/weechat_dev.en.html#color_codes_in_strings'."
  :type '(repeat (choice (const :tag "Unspecified" unspecified)
                         (const :tag "Color" color)))
  :group 'weechat)

(defvar weechat-color-attributes-alist '((?* . (:weight bold)) (?\1 . (:weight bold))         ; bold
                                         (?! . (:inverse-video t)) (?\2 . (:inverse-video t)) ; reverse??
                                         (?/ . (:slant italic)) (?\3 . (:slant italic))       ; italic
                                         (?_ . (:underline t)) (?\4 . (:underline t))         ; underline
                                         (?| . keep))                                         ; keep
  "Map color attribute specifiers to Emacs face property.")


(defvar weechat-terminal-colors
              (concat
               "000000cd000000cd00cdcd000000cdcd00cd00cdcde5e5e5"
               "4d4d4dff000000ff00ffff000000ffff00ff00ffffffffff"
               "00000000002a0000550000800000aa0000d4002a00002a2a"
               "002a55002a80002aaa002ad400550000552a005555005580"
               "0055aa0055d400800000802a0080550080800080aa0080d4"
               "00aa0000aa2a00aa5500aa8000aaaa00aad400d40000d42a"
               "00d45500d48000d4aa00d4d42a00002a002a2a00552a0080"
               "2a00aa2a00d42a2a002a2a2a2a2a552a2a802a2aaa2a2ad4"
               "2a55002a552a2a55552a55802a55aa2a55d42a80002a802a"
               "2a80552a80802a80aa2a80d42aaa002aaa2a2aaa552aaa80"
               "2aaaaa2aaad42ad4002ad42a2ad4552ad4802ad4aa2ad4d4"
               "55000055002a5500555500805500aa5500d4552a00552a2a"
               "552a55552a80552aaa552ad455550055552a555555555580"
               "5555aa5555d455800055802a5580555580805580aa5580d4"
               "55aa0055aa2a55aa5555aa8055aaaa55aad455d40055d42a"
               "55d45555d48055d4aa55d4d480000080002a800055800080"
               "8000aa8000d4802a00802a2a802a55802a80802aaa802ad4"
               "80550080552a8055558055808055aa8055d480800080802a"
               "8080558080808080aa8080d480aa0080aa2a80aa5580aa80"
               "80aaaa80aad480d40080d42a80d45580d48080d4aa80d4d4"
               "aa0000aa002aaa0055aa0080aa00aaaa00d4aa2a00aa2a2a"
               "aa2a55aa2a80aa2aaaaa2ad4aa5500aa552aaa5555aa5580"
               "aa55aaaa55d4aa8000aa802aaa8055aa8080aa80aaaa80d4"
               "aaaa00aaaa2aaaaa55aaaa80aaaaaaaaaad4aad400aad42a"
               "aad455aad480aad4aaaad4d4d40000d4002ad40055d40080"
               "d400aad400d4d42a00d42a2ad42a55d42a80d42aaad42ad4"
               "d45500d4552ad45555d45580d455aad455d4d48000d4802a"
               "d48055d48080d480aad480d4d4aa00d4aa2ad4aa55d4aa80"
               "d4aaaad4aad4d4d400d4d42ad4d455d4d480d4d4aad4d4d4"
               "0808081212121c1c1c2626263030303a3a3a4444444e4e4e"
               "5858586262626c6c6c7676768080808a8a8a9494949e9e9e"
               "a8a8a8b2b2b2bcbcbcc6c6c6d0d0d0dadadae4e4e4eeeeee"))

(defun weechat--match-color-code (what str i)
  "Match std, ext, attr WHAT on STR at position I.
This is internal and used by `weechat-handle-color-codes'."
  (when (symbolp what)
    (cl-case what
      ((std)
       (let ((std (substring str i (+ i 2))))
         (when (s-matches? "^[0-9]+$" std)
           (list 'std (+ i 2) (string-to-number std)))))
      ((ext)
       (when (= (aref str i) ?@)
         (let ((std (substring str (1+ i) (+ i 6))))
           (when (s-matches? "^[0-9]+$" std)
             (list 'ext (+ i 6) (string-to-number std))))))
      ((attr)
       (let* ((a (aref str i))
              (x (cdr (assq a weechat-color-attributes-alist))))
         (when x
           (list 'attr (1+ i) x))))
      (t (error "Unknown parameter %s" what)))))

(defun weechat--color-keep-attributes (old-face)
  "Remove color settings from OLD-FACE but keep the attributes."
  (cl-delete-if (lambda (x)
                  (memq (car x) '(:foreground :background)))
                old-face))

(defun weechat--rgb-color (index)
    (let* ((colors (substring weechat-terminal-colors (* index 6)))
           (r (* 0.85 (string-to-number (substring colors 0 2) 16)))
           (g (* 0.85 (string-to-number (substring colors 2 4) 16)))
           (b (* 0.85 (string-to-number (substring colors 4 6) 16))))
      (format "#%02x%02x%02x" r g b)))

(defun weechat--color-handle-F (str i old-face)
  "Handle ?F (A)STD|(A)EXT color code in STR at I with OLD-FACE.
This is an internal function of `weechat-handle-color-codes'."
  (let (match-data
        face
        (j (1+ i)))
    (while (setq match-data (weechat--match-color-code 'attr str j)) ;; (A)
      (if (eq (cl-third match-data) 'keep)
          (setq face (weechat--color-keep-attributes old-face))
        (setq face (append (list (cl-third match-data)) face)))
      (setq j (cl-second match-data)))
    (setq match-data (weechat--match-color-code 'std str j))
    (if match-data
        (setq face (append (list (list :foreground (nth (cl-third match-data)
                                                        weechat-color-list)))
                           face)) ;; TODO set attribute instead of simply append
      (setq match-data (weechat--match-color-code 'ext str j))
      (if match-data
          (setq face (list (list :foreground (weechat--rgb-color (cl-third match-data)))))
        (weechat-relay-log (format "Broken color code (in ?F '%s' %s)" str i)
                           :warn)))
    (if match-data
        (cl-values (cl-second match-data)
                   face)
      (cl-values j face))))

(defvar weechat-color-options-list
  '(("weechat.color.separator" . "blue") ;; KEEP THE ORDER!
    ("weechat.color.chat" . default)
    ("weechat.color.chat_time" . weechat-time-face)
    ("weechat.color.chat_time_delimiters" . weechat-time-face)
    ("weechat.color.chat_prefix_error" . "yellow")
    ("weechat.color.chat_prefix_network" . "magenta")
    ("weechat.color.chat_prefix_action" . "white")
    ("weechat.color.chat_prefix_join" . "light green")
    ("weechat.color.chat_prefix_quit" . "orange red") ;; light red
    ("weechat.color.chat_prefix_more" . "medium violet red") ;; light magenta
    ("weechat.color.chat_prefix_suffix" . "green")
    ("weechat.color.chat_buffer" . "white")
    ("weechat.color.chat_server" . "brown")
    ("weechat.color.chat_channel" . "white")
    ("weechat.color.chat_nick" . "light cyan")
    ("weechat.color.chat_nick_self" . weechat-nick-self-face)
    ("weechat.color.chat_nick_other" . "cyan")
    (nil . default)
    (nil . default)
    (nil . default)
    (nil . default)
    (nil . default)
    (nil . default)
    (nil . default)
    (nil . default)
    (nil . default)
    (nil . default)
    ("weechat.color.chat_host" . "cyan")
    ("weechat.color.chat_delimiters" . "green")
    ("weechat.color.chat_highlight" . weechat-highlight-face)
    ("weechat.color.chat_read_marker" . "magenta")
    ("weechat.color.chat_text_found" . "yellow")
    ("weechat.color.chat_value" . "cyan")
    ("weechat.color.chat_prefix_buffer")
    ("weechat.color.chat_tags" . "red")
    ("weechat.color.chat_inactive_window" . "dark grey")
    ("weechat.color.chat_inactive_buffer" . "dark grey")
    ("weechat.color.chat_prefix_buffer_inactive_buffer" . "dark grey")
    ("weechat.color.chat_nick_offline" . "dark grey")
    ("weechat.color.chat_nick_offline_highlight" . default))
  "List of color options for \x19XX.")
;; TODO every option here should probably be a face!

(defun weechat--color-std-to-theme (num)
  "Turn color code in NUM using option into face."
  (if (or (not (integerp num))
          (> num (length weechat-color-options-list)))
      'default
    (let ((face (cdr (nth num weechat-color-options-list))))
      (if (stringp face) ;; color value
          (list (list :foreground face ))
        face))))

(defun weechat-handle-color-codes (str)
  "Convert the Weechat color codes in STR to properties.
EXT colors are currently not supported.  Any color codes left are stripped.

Be aware that Weechat does not use mIRC color codes.
See URL `http://www.weechat.org/files/doc/devel/weechat_dev.en.html#color_codes_in_strings'."
  (let ((i 0)
        face
        (ret "")
        (len (length str)))
    (while (< i len)
      (cl-case (aref str i)
        ((?\x19) ;; STD|EXT|?F((A)STD|(A)EXT)|?B(STD|EXT)|?\x1C|?*...|?b...
         (let ((old-face face)
               (next (aref str (1+ i))))
           (setq face nil)
           (setq i (1+ i))
           (cond
            ((and (<= ?0 next) (<= next ?9)) ;; STD
             (let ((match-data (weechat--match-color-code 'std str i)))
               (when match-data
                 (setq face (weechat--color-std-to-theme (cl-third match-data)))
                 (setq i (cl-second match-data)))))
            ((= next ?@) ;; EXT
             (let ((match-data (weechat--match-color-code 'ext str i)))
               (when match-data
                 ;; TODO
                 (setq i (cl-second match-data)))))
            ((= next ?F) ;; ?F(A)STD|?F(A)EXT
             (cl-multiple-value-setq (i face) (weechat--color-handle-F str i old-face)))
            ((= next ?B) ;; ?BSTD|?BEXT
             (let ((match-data (weechat--match-color-code 'std str i)))
               (if match-data
                   (setq face (list (list :background (nth (cl-third match-data)
                                                           weechat-color-list))))
                 (setq match-data (weechat--match-color-code 'ext str i))
                 (if match-data
                     t ;; TODO ext
                   (weechat-relay-log (format "Broken color code (in ?B '%s' %s)" str i)
                                      :warn)))
               (setq i (if match-data
                           (cl-second match-data)
                         (1+ i)))))
            ((= next ?*) ;; (A)STD | (A)EXT | (A)STD ?, (A)STD | ...
             (cl-multiple-value-setq (i face) (weechat--color-handle-F str i old-face))
             (if (= (aref str i) ?,)
                 (let* ((i (1+ i))
                        (match-data (weechat--match-color-code 'std str i)))
                   (if match-data
                       (setq face (append (list (list :background (nth (cl-third match-data)
                                                                       weechat-color-list)))
                                          face))
                     (setq match-data (weechat--match-color-code 'ext str i))
                     (if match-data
                         t ;; TODO ext
                       (weechat-relay-log (format "Broken color code (in ?* '%s' %s)" str i)
                                          :warn)))
                   (setq i (if match-data
                               (cl-second match-data)
                             (1+ i))))))
            ((= next ?b) 'b) ;; ignore for now
            ((= next ?\x1C)  ;; Clear color, leave attributes
             (setq face (weechat--color-keep-attributes old-face))))))

        ((?\x1A) ;; Set ATTR
         (let ((match-data (weechat--match-color-code 'attr str (1+ i))))
           (if (not match-data)
               (progn
                 (weechat-relay-log (format "Broken color code (in ?\\x1A '%s' %s)" str i)
                                    :warn)
                 (setq i (1+ i)))
             (if (eq (cl-third match-data) 'keep)
                 (setq face (weechat--color-keep-attributes face))
               (setq face (list (cl-third match-data))))
             (setq i (cl-second match-data)))))

        ((?\x1B) ;; Delete ATTR
         (let ((match-data (weechat--match-color-code 'attr str (1+ i)))
               (old-face (copy-sequence face)))
           (if (not match-data)
               (progn
                 (weechat-relay-log (format "Broken color code (in ?\\x1B '%s' %s)" str i)
                                    :warn)
                 (setq i (1+ i)))
             (if (eq (cl-third match-data) 'keep)
                 (setq face nil) ;; TODO Does keep here means delete all or keep all?
               (setq face (delq (cl-third match-data) old-face)))
             (setq i (cl-second match-data)))))

        ((?\x1C) (setq i (1+ i) face nil))) ;; reset face

      (let ((r (string-match-p "\\(\x19\\|\x1A\\|\x1B\\|\x1C\\)" str i)))
        (if r
            (setq ret (concat ret
                              (propertize  (substring str i r) 'face (or face 'default)))
                  i r)
          (setq ret (concat ret (propertize (substring str i) 'face (or face 'default)))
                i len)))) ;; STOP
    ret))

(provide 'weechat-color)

;;;  weechat-color.el ends here
