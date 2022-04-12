
(require 'ert)
(require 'evil)
(require 'evil-surround)
(require 'evil-test-helpers)

;; interactive test helper (this is the standard configuration)
(evil-add-to-alist
 'evil-surround-pairs-alist
 ?\) '("(" . ")")
 ?\] '("[" . "]")
 ?\} '("{" . "}")
 ?\( '("( " . " )")
 ?\[ '("[ " . " ]")
 ?\{ '("{ " . " }"))

(defmacro test-widened-buffer (start cmds exp)
  (declare (indent 0))
  `(let (widened)
     (evil-test-buffer
       ,start
       (turn-on-evil-surround-mode)
       (cl-letf (((symbol-function #'widen)
                  (lambda () (setq widened t))))
         (execute-kbd-macro ,(car cmds)))
       ,exp)
     (should widened)))

(ert-deftest evil-surround-test ()
  (ert-info ("basic surrounding")
    (evil-test-buffer
      "one [t]wo three"
      (turn-on-evil-surround-mode)
      ("ysiwb")
      "one (two) three"
      ("csb'")
      "one 'two' three"
      ("ds'")
      "one two three"))
  (ert-info ("shortcut surrounding")
    (evil-test-buffer
      "One, and two. Also three.\n\n"
      (turn-on-evil-surround-mode)
      ("csw)")
      "(One), and two. Also three.\n\n"
      ("csW'")
      "'(One),' and two. Also three.\n\n"
      ("css.")
      ".'(One),' and two. Also three..\n\n"
      ("csp0")
      "0.'(One),' and two. Also three..0\n\n"))
  (ert-info ("examples from readme")
    (evil-test-buffer
      :visual-start nil
      :visual-end nil
      "\"Hello world!\""
      (turn-on-evil-surround-mode)
      ("cs\"'")
      "'Hello world!'"
      ("cs'<q>")
      "<q>Hello world!</q>"
      ("cst\"")
      "\"Hello world!\""
      ("ds\"")
      "Hello world!"
      ("ysiw]")
      "[Hello] world!"
      ("cs[{")
      "{ Hello } world!"
      ("yssb")
      "({ Hello } world!)"
      ("lds{ds)") ;; 'l' to move the cursor right, inside brackets
      "Hello world!"
      ("ysiw<em>")
      "<em>Hello</em> world!"))
  (ert-info ("tests for dots and caps support in tags")
	(evil-test-buffer
	 :visual-start nil
	 :visual-end nil
	 "\"Hello world!\""
	 (turn-on-evil-surround-mode)
	 ("cs\"'")
	 "'Hello world!'"
	 ("cs'<Table.Hi>")
	 "<Table.Hi>Hello world!</Table.Hi>"
	 ("dst")
	 "Hello world!"
	 ("ysiw<div.Test attr=\"true\">")
	 "<div.Test attr=\"true\">Hello</div.Test> world!"
	 ("cst<Three.Separate.Components>")
	 "<Three.Separate.Components>Hello</Three.Separate.Components> world!"))
  (ert-info ("more examples from readme: function surrounding with dot repeat")
    (evil-test-buffer
      :visual-start nil
      :visual-end nil
      "argument1 argument2"
      (turn-on-evil-surround-mode)
      ("ysiwffunction" [return])
      "function(argument1) argument2"
      ("W.")
      "function(argument1) function(argument2)"))
  (ert-info ("prefix-function surrounding with dot repeat")
    (evil-test-buffer
      :visual-start nil
      :visual-end nil
      "argument1 argument2"
      (turn-on-evil-surround-mode)
      ("ysiw\C-ffunction" [return])
      "(function argument1) argument2"
      ("WW.")
      "(function argument1) (function argument2)"))
  (ert-info ("even more examples from readme: tag surrounding with dot repeat")
    (evil-test-buffer
      :visual-start nil
      :visual-end nil
      "tag1 tag2"
      (turn-on-evil-surround-mode)
      ("ysiw<a>")
      "<a>tag1</a> tag2"
      ("W.")
      "<a>tag1</a> <a>tag2</a>"))
  (ert-info ("optionally keep xml attributes")
    (evil-test-buffer
      :visual-start nil
      :visual-end nil
      "<div class=\"foo\">Bar</div>"
      (turn-on-evil-surround-mode)
      ("cst<span")
      "<span class=\"foo\">Bar</span>"
      ("cst<p>")
      "<p>Bar</p>"))
  (ert-info ("optionally keep xml attributes: more complicated cases")
    (evil-test-buffer
      :visual-start nil
      :visual-end nil
      "<div ngModel class=\"foo\" randomAngularDirective #anchor1>Bar</div>"
      (turn-on-evil-surround-mode)
      ("cst<span")
      "<span ngModel class=\"foo\" randomAngularDirective #anchor1>Bar</span>"
      ("cst<p>")
      "<p>Bar</p>"))
  (ert-info ("optionally keep xml attributes: repeating")
    (evil-test-buffer
     :visual-start nil
     :visual-end nil
     "<div attr=\"foo\">Foo</div><div attr=\"bar\">Bar</div>"
     (turn-on-evil-surround-mode)
     ("cst<span")
     "<span attr=\"foo\">Foo</span><div attr=\"bar\">Bar</div>"
     ("fB.")
     "<span attr=\"foo\">Foo</span><span attr=\"bar\">Bar</span>"))
  (ert-info ("repeat surrounding")
    (evil-test-buffer
      "[o]ne two three"
      (turn-on-evil-surround-mode)
      ;; surround and repeat it
      ("ysiwb")
      "(one) two three"
      ("W.") ;; repeat surround region
      "(one) (two) three"
      ("W.") ;; repeat surround region
      "(one) (two) (three)"

      ;; change surround and repeat it
      ("0csb'")
      "'one' (two) (three)"
      ("W.") ;; repeat change surround
      "'one' 'two' (three)"
      ("W.") ;; repeat change surround
      "'one' 'two' 'three'"

      ;; delete surround and repeat it
      ("0ds'")
      "one 'two' 'three'"
      ("W.") ;; repeat delete surround
      "one two 'three'"
      ("W.") ;; repeat delete surround
      "one two three"))
  (ert-info ("repeat surrounding with count")
    (evil-test-buffer
      "[o]ne two three\none two three"
      (turn-on-evil-surround-mode)
      ("ys2wb")
      "(one two) three\none two three"
      ("j.")
      "(one two) three\n(one two) three"))
  (ert-info ("visual surrounding")
    (evil-test-buffer
      "<one two> three\nfour\n"
      (turn-on-evil-surround-mode)
      ("Sb")
      "(one two) three\nfour\n")
    (evil-test-buffer
      "<one two three>\nfour\n"
      (turn-on-evil-surround-mode)
      ("Sb")
      "(one two three)\nfour\n")
    (evil-test-buffer
      "<one two three\nfo>ur\n"
      (turn-on-evil-surround-mode)
      ("Sb")
      "(one two three\nfo)ur\n"))
  (ert-info ("visual line surrounding")
    (evil-test-buffer
      "[o]ne two three\nfour\n"
      (turn-on-evil-surround-mode)
      ("yssb")
      "(one two three)\nfour\n"
      ("dsb")
      "one two three\nfour\n"
      ("VSb")
      "(\none two three\n)\nfour\n")
    (evil-test-buffer
      "111 222 333\n[1]11 222 333\n111 222 333\n111 222 333\n"
      (turn-on-evil-surround-mode)
      ("ysjb")
      "111 222 333\n(\n111 222 333\n111 222 333\n)\n111 222 333\n"))
  (ert-info ("test with evil-want-change-word-to-end")
    (evil-test-buffer
      "[o]ne    two  three"
      (setq evil-want-change-word-to-end nil)
      (turn-on-evil-surround-mode)
      ("yswb")
      "[(]one    )two  three"
      ("dsb")
      "[o]ne    two  three"
      ("ys2wb")
      "[(]one    two  )three"
      ("dsb")
      "[o]ne    two  three"
      ("ys3wb")
      "[(]one    two  three)")
    (evil-test-buffer
      "[o]ne    two  three"
      (setq evil-want-change-word-to-end t)
      (turn-on-evil-surround-mode)
      ("yswb")
      "[(]one)    two  three"
      ("dsb")
      "[o]ne    two  three"
      ("ys2wb")
      "[(]one    two)  three"
      ("dsb")
      "[o]ne    two  three"
      ("ys3wb")
      "[(]one    two  three)"))
  (ert-info ("yS test")
    (evil-test-buffer
      "some_word"
      (turn-on-evil-surround-mode)
      ("ySiW\"")
      "\"\nsome_word\n\""
      ))
  (ert-info ("ensure backquote delimiters work")
    (evil-test-buffer
      "`this_is_a_[b]acktick_surrounded_word`"
      (turn-on-evil-surround-mode)
      ("cs`)")
      "[(]this_is_a_backtick_surrounded_word)"
      ))
  (ert-info ("buffer is widened before reading char")
    (test-widened-buffer
     "`[w]ord`"
     ("cs`)")
     "[(]word)")
    (test-widened-buffer
     "`[w]ord`"
     ("ds`")
     "[w]ord")
    (test-widened-buffer
     "[w]ord"
     ("ysiwb")
     "[(]word)"
     ))
  (ert-info ("create buffer local text object and surround binding")
    (evil-test-buffer
     "QQw[o]rdQQ soup"
     (turn-on-evil-surround-mode)
     (evil-define-text-object evil-surround-test-inner-QQ (count &optional beg end type)
       "Test text object"
       :extend-selection nil
       (evil-select-paren "QQ" "QQ"
                          beg end type count nil))
     (evil-define-text-object evil-surround-test-a-QQ (count &optional beg end type)
       :extend-selection nil
       (evil-select-paren "QQ" "QQ"
                          beg end type count t))
     (evil-define-text-object evil-surround-test-inner-NN (count &optional beg end type)
       "Test text object"
       :extend-selection nil
       (evil-select-paren "NN" "NN"
                          beg end type count nil))
     (evil-define-text-object evil-surround-test-a-NN (count &optional beg end type)
       :extend-selection nil
       (evil-select-paren "NN" "NN"
                          beg end type count t))

     (defvar evil-surround-test-inner-text-objects-map (make-sparse-keymap)
       "Inner text object test keymap")
     (defvar evil-surround-test-outer-text-objects-map (make-sparse-keymap)
       "Outer text object keymap")
     (define-key evil-surround-test-inner-text-objects-map "Q" #'evil-surround-test-inner-QQ)
     (define-key evil-surround-test-outer-text-objects-map "Q" #'evil-surround-test-a-QQ)
     (define-key evil-visual-state-local-map   "iQ" #'evil-surround-test-inner-QQ)
     (define-key evil-operator-state-local-map "iQ" #'evil-surround-test-inner-QQ)
     (define-key evil-visual-state-local-map   "aQ" #'evil-surround-test-a-QQ)
     (define-key evil-operator-state-local-map "aQ" #'evil-surround-test-a-QQ)
     (define-key evil-visual-state-local-map   "iN" #'evil-surround-test-inner-NN)
     (define-key evil-operator-state-local-map "iN" #'evil-surround-test-inner-NN)
     (define-key evil-visual-state-local-map   "aN" #'evil-surround-test-a-NN)
     (define-key evil-operator-state-local-map "aN" #'evil-surround-test-a-NN)
     (setq evil-surround-local-inner-text-object-map-list (list evil-surround-test-inner-text-objects-map ))
     (setq evil-surround-local-outer-text-object-map-list (list evil-surround-test-outer-text-objects-map ))
     (setq evil-surround-local-inner-text-object-map-list (list (lookup-key evil-operator-state-local-map "i")))
     (setq evil-surround-local-outer-text-object-map-list (list (lookup-key evil-operator-state-local-map "a")))
     (setq-local evil-surround-pairs-alist (append '((?Q "QQ" . "QQ") (?N "NN" . "NN")) evil-surround-pairs-alist))
     ("dsQ")
     "word soup"
     ("cswQ")
     "QQwordQQ soup"
     ("daQ")
     " soup"
     ("ysawQ")
     "QQ soupQQ"
     ("csQb")
     "( soup)"
     ("csbN")
     "NN soupNN"
     ))
  (ert-info ("check that the previous binding was in fact local")
    (evil-test-buffer
     "QQw[o]rdQQ soup"
     (turn-on-evil-surround-mode)
     ("dsQ")
     "QQwordQQ soup"
     ("cswQ")
     "QQQwordQQQ soup")))


(ert-deftest evil-surround-tag-from-macro ()
 (ert-info ("tag surround in macro")
   (save-window-excursion
     (with-current-buffer (get-buffer-create "*evil-surround-tag-from-macro*")
       (switch-to-buffer-other-window (current-buffer))
       (insert "foo\nbar\nbaz\n")
       (goto-char (point-min))
       (evil-mode 1)
       (turn-on-evil-surround-mode)
       (execute-kbd-macro
        "yse<div>f>lysit$j_")
       (should
        (string= "<div>$foo$</div>\nbar\nbaz\n"
                 (buffer-substring-no-properties (point-min) (point-max))))))))
