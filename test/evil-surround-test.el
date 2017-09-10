
(require 'ert)
(require 'evil)
(require 'evil-surround)
(require 'evil-test-helpers)

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
      "<em>Hello</em> world!")))
