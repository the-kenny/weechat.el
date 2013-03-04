(require 'weechat)
(require 'ert)

;;; weechat-relay.el

(defmacro weechat-test-callback-value (command)
  "Execute COMMAND"
  (let ((data-sym (gensym "data"))
        (limit-sym (gensym "limit"))
        (id (symbol-name (gensym "id"))))
    `(let (,data-sym
           (,limit-sym 200))
       (weechat-relay-add-id-callback ,id (lambda (d) (setq ,data-sym d)) 'one-shot)
       (weechat--relay-send-message ,command ,id)
       (while (and (> ,limit-sym 0) (not ,data-sym))
         (sleep-for 0 50)
         (setq ,limit-sym (1- ,limit-sym)))
       ,data-sym)))

(ert-deftest weechat-relay-id-callback ()
  (let ((weechat--relay-id-callback-hash
         (copy-hash-table weechat--relay-id-callback-hash)))
    (let ((fun (lambda (_) nil)) )
      (weechat-relay-add-id-callback "23" fun)
      (should (equal fun (weechat-relay-get-id-callback "23")))
      (should (equal fun (weechat-relay-remove-id-callback "23"))))
    (clrhash weechat--relay-id-callback-hash)
    (should-error (progn (weechat-relay-add-id-callback "42" (lambda ()))
                         (weechat-relay-add-id-callback "42" (lambda ()))))))

(ert-deftest weechat-relay-id-callback-one-shot ()
  (let ((weechat--relay-id-callback-hash
         (copy-hash-table weechat--relay-id-callback-hash)))
    (let ((fun (lambda (_) nil)))
      (weechat-relay-add-id-callback "23" fun 'one-shot)
      (funcall (weechat-relay-get-id-callback "23") nil)
      (should (equal nil (weechat-relay-get-id-callback "23"))))))

(ert-deftest weechat-test-message-fns ()
  (let ((message '("42" ("version" . "0.3.8"))))
    (should (equal "42" (weechat--message-id message)))
    (should (equal '("version" . "0.3.8") (car (weechat--message-data message))))))

(ert-deftest weechat-test-hdata-fns ()
  (let ((hdata '("foo/bar"
                 ((("0x155f870" "0xffffff")
                   ("title" . "IRC: irc.euirc.net/6667 (83.137.41.33)")
                   ("short_name" . "euirc")
                   ("name" . "server.euirc"))
                  (("0x1502940")
                   ("title" . "IRC: irc.freenode.net/6697 (174.143.119.91)")
                   ("short_name" . "freenode")
                   ("name" . "server.freenode"))))))
    (should (equal "foo/bar" (weechat--hdata-path hdata)))
    (should (listp (weechat--hdata-values hdata)))
    (should (equal '(("0x155f870" "0xffffff") ("0x1502940"))
                   (mapcar #'weechat--hdata-value-pointer-path (weechat--hdata-values hdata))))
    (should (equal '((("title" . "IRC: irc.euirc.net/6667 (83.137.41.33)")
                      ("short_name" . "euirc")
                      ("name" . "server.euirc"))
                     (("title" . "IRC: irc.freenode.net/6697 (174.143.119.91)")
                      ("short_name" . "freenode")
                      ("name" . "server.freenode")))
                   (mapcar #'weechat--hdata-value-alist (weechat--hdata-values hdata))))))

(ert-deftest weechat-test-infolist ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [0 0 0 32 0 255 255 255 255 105 110 102 0 0
                       0 7 118 101 114 115 105 111 110 0 0 0 5
                       48 46 51 46 56]))
    (let ((data (weechat--relay-parse-new-message (current-buffer))))
      (should (equal ""  (weechat--message-id data)))
      (should (equal '("version" . "0.3.8")
                     (car (weechat--message-data data)))))))


(ert-deftest weechat-test-id ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (concat [0 0 0 35 0 0 0 0 3 54 54 54 105 110 102 0
                       0 0 7 118 101 114 115 105 111 110 0 0 0
                       5 48 46 51 46 56]))
    (let ((data (weechat--relay-parse-new-message (current-buffer))))
      (should (equal "666" (weechat--message-id data)))
      (should (equal '("version" . "0.3.8")
                     (car (weechat--message-data data)))))))

(ert-deftest weechat-relay-test-connection ()
  (when (weechat-relay-connected-p)
    (let ((version-resp (weechat-test-callback-value "info version")))
      (should (equal "version" (caar version-resp)))
      (should (equal weechat-version (cdar version-resp))))))

(ert-deftest weechat-relay-test-test-command ()
  (when (weechat-relay-connected-p)
    (let ((data (weechat-test-callback-value "test"))
          (i -1))
      (cl-flet ((next-val () (nth (setq i (1+ i)) data)))
        (should (equal ?A                           (next-val)))
        (should (equal 123456                       (next-val)))
        (should (equal 1234567890                   (next-val)))
        (should (equal "a string"                   (next-val)))
        (should (equal ""                           (next-val)))
        (should (equal ""                           (next-val)))
        (should (equal [98 117 102 102 101 114]     (next-val)))
        (should (equal []                           (next-val)))
        (when (version< "0.4.0" weechat-version)
          (should (equal "0x1234abcd"               (next-val))))
        (when (version<= weechat-version "0.4.1")
          (should (equal nil                        (next-val))))
        (should (equal (seconds-to-time 1321993456) (next-val)))
        (should (equal '("abc" "de")                (next-val)))
        (should (equal '(123 456 789)               (next-val)))))))

;;; weechat.el

(ert-deftest weechat-test-buffer-store ()
  (let ((weechat--buffer-hashes (copy-hash-table weechat--buffer-hashes)))
    (weechat--clear-buffer-store)
    (should (eql 0 (hash-table-count weechat--buffer-hashes)))
    (let ((data '(("name" . "Foobar"))))
      (weechat--store-buffer-hash "0xffffff" data)
      (should (eq (cdar data)
                  (gethash "name" (weechat-buffer-hash "0xffffff")))))
    (weechat--remove-buffer-hash "0xffffff")
    (should (not (weechat-buffer-hash "0xffffff")))))

(ert-deftest weechat-color-stripping ()
  (should (equal (weechat-strip-formatting
                  "F14someone282728F05 has joined 13#asdfasdfasdfF05")
                 "someone has joined #asdfasdfasdf"))
  (should (equal (weechat-strip-formatting "ddd") "ddd")))

(defun weechat-test--property-list (str &optional prop pos)
  "Return a list of property PROP in STR starting at POS.
Default property is `face'.  The returned format is ((START END (PROP VALUE)))."
  (setq pos (or pos 0))
  (setq prop (or prop 'face))
  (let ((next-pos pos)
        result)
    (while pos
      (setq next-pos (next-single-property-change pos prop str))
      (setq result (cons (list pos (or next-pos (length str))
                               (list prop
                                     (get-text-property pos prop str)))
                         result))
      (setq pos next-pos))
    result))

(ert-deftest weechat-color-handling ()
  "Test `weechat-handle-color-codes'."
  (should (string= (weechat-handle-color-codes "foo bar baz")
                   "foo bar baz"))
  (should (string= (weechat-handle-color-codes "\x19\F*02hi\x1C \x19\F/04world")
                   "hi world"))
  (should (equal (weechat-test--property-list
                  (weechat-handle-color-codes "\x19\F*02hi\x1C \x19\F/04world"))
                 '((3 8
                      (face
                       ((:foreground "red")
                        (:slant italic))))
                   (2 3
                      (face default))
                   (0 2
                      (face
                       ((:foreground "dark gray")
                        (:weight bold)))))))
  (should (string= (weechat-handle-color-codes "\x19\Fkaputt") "kaputt"))
  (should (string= (weechat-handle-color-codes "XY\x1A\Z") "XYZ"))
  (should (string= (weechat-handle-color-codes "\x1Bx") "x")))

(ert-deftest weechat-alist-merging ()
  (should (equal '((x . 42)) (weechat-merge-alists '((x . 23)) '((x . 42)))))
  (should (equal '(("x" . 42)) (weechat-merge-alists '(("x" . 23)) '(("x" . 42)))))
  (should (equal '((x . 42)) (weechat-merge-alists '() '((x . 42)))))
  (should (equal '((x . 42)) (weechat-merge-alists '((x . 42)) '()))))

(ert-deftest weechat-user-list ()
  (let ((weechat-user-list))
    (weechat--user-list-add "test")
    (should (equal weechat-user-list '("test")))
    (weechat--user-list-add "test")
    (should (equal weechat-user-list '("test")))
    (weechat--user-list-remove "notthere")
    (should (equal weechat-user-list '("test")))
    (weechat--user-list-add "test2")
    (should (equal weechat-user-list '("test2" "test")))
    (weechat--user-list-add "test_")
    (should (equal weechat-user-list '("test_" "test2" "test")))
    (weechat--user-list-remove "test2")
    (should (equal weechat-user-list '("test_" "test")))
    (weechat--user-list-remove "test")
    (should (equal weechat-user-list '("test_")))
    (weechat--user-list-remove "test_")
    (should (eq weechat-user-list nil))
    (weechat--user-list-add "")
    (should (eq weechat-user-list nil))
    (weechat--user-list-add "x")
    (should (equal weechat-user-list '("x")))
    (weechat--user-list-add "")
    (should (equal weechat-user-list '("x")))))
