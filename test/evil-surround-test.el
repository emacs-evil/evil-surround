
(require 'ert)
(require 'evil)
(require 'evil-surround)

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
      "one two three")))

(ert-deftest evil-surround-dot-test ()
  (ert-info ("basic surrounding")
    (evil-test-buffer
      "one ((([t]wo))) three"
      (turn-on-evil-surround-mode)
      ("cs)]")
      "one (([two])) three"
      (".")
      "one ([[two]]) three"
      (".")
      "one [[[two]]] three")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code below is copied from evil-tests.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil-test-point nil
  "Marker for point.")
(make-variable-buffer-local 'evil-test-point)
(defvar evil-test-visual-start nil
  "Marker for Visual beginning.")
(make-variable-buffer-local 'evil-test-visual-start)
(defvar evil-test-visual-end nil
  "Marker for Visual end.")
(make-variable-buffer-local 'evil-test-visual-end)


(defmacro evil-test-buffer (&rest body)
  "Execute FORMS in a temporary buffer.
The following optional keywords specify the buffer's properties:
:state STATE            The initial state, defaults to `normal'.
:visual SELECTION       The Visual selection, defaults to `char'.
:point-start STRING     String for matching beginning of point,
                        defaults to \"[\".
:point-end STRING       String for matching end of point,
                        defaults to \"]\".
:visual-start STRING    String for matching beginning of
                        Visual selection, defaults to \"<\".
:visual-end STRING      String for matching end of
                        Visual selection, defaults to \">\".
Then follows one or more forms. If the first form is a string,
it is taken to be a buffer description as passed to
`evil-test-buffer-from-string', and initializes the buffer.
Subsequent string forms validate the buffer.
If a form is a list of strings or vectors, it is taken to be a
key sequence and is passed to `execute-kbd-macro'.  If the form
is \(file FILENAME CONTENTS), then the test fails unless the
contents of FILENAME equal CONTENTS.  If the form is \(error
SYMBOL ...) then the test fails unless an error of type SYMBOL is
raised.  Remaining forms are evaluated as-is.
\(fn [[KEY VALUE]...] FORMS...)"
  (declare (indent defun))
  (let ((state 'normal)
        arg key point-start point-end string
        visual visual-start visual-end)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :point-start)
        (setq point-start (or arg "")))
       ((eq key :point-end)
        (setq point-end (or arg "")))
       ((eq key :state)
        (setq state arg))
       ((eq key :visual)
        (setq visual arg))
       ((eq key :visual-start)
        (setq visual-start (or arg "")))
       ((eq key :visual-end)
        (setq visual-end (or arg "")))))
    ;; collect buffer initialization
    (when (stringp (car-safe body))
      (setq string (pop body)))
    ;; macro expansion
    `(let ((buffer (evil-test-buffer-from-string
                    ,string ',state
                    ,point-start ,point-end
                    ',visual ,visual-start ,visual-end))
           (kill-ring kill-ring)
           (kill-ring-yank-pointer kill-ring-yank-pointer)
           x-select-enable-clipboard
           message-log-max)
       (unwind-protect
           (save-window-excursion
             (with-current-buffer buffer
               ;; necessary for keyboard macros to work
               (switch-to-buffer-other-window (current-buffer))
               (buffer-enable-undo)
               (undo-tree-mode 1)
               ;; parse remaining forms
               ,@(mapcar
                  #'(lambda (form)
                      (let (error-symbol)
                        (when (and (listp form)
                                   (eq (car-safe form) 'error))
                          (setq error-symbol (car-safe (cdr-safe form))
                                form (cdr-safe (cdr-safe form))))
                        (let ((result
                               (cond
                                ((stringp form)
                                 `(evil-test-buffer-string
                                   ,form
                                   ',point-start ',point-end
                                   ',visual-start ',visual-end))
                                ((eq (car-safe form) 'file)
                                 `(evil-test-file-contents ,(cadr form)
                                                           ,(caddr form)))
                                ((or (stringp (car-safe form))
                                     (vectorp (car-safe form))
                                     (memq (car-safe (car-safe form))
                                           '(kbd vconcat)))
                                 ;; we need to execute everything as a single
                                 ;; sequence for command loop hooks to work
                                 `(execute-kbd-macro
                                   (apply #'vconcat
                                          (mapcar #'listify-key-sequence
                                                  (mapcar #'eval ',form)))))
                                ((memq (car-safe form) '(kbd vconcat))
                                 `(execute-kbd-macro ,form))
                                (t
                                 form))))
                          (if error-symbol
                              `(should-error ,result :type ',error-symbol)
                            result))))
                  body)))
         (and (buffer-name buffer)
              (kill-buffer buffer))))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(\\(evil-test-buffer\\)\\>"
                             1 font-lock-keyword-face))))

(defun evil-test-buffer-string (string &optional
                                       point-start point-end
                                       visual-start visual-end)
  "Validate the current buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, that position is compared against point.
If STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, those positions are compared against the Visual selection.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >."
  (let ((actual-buffer (current-buffer))
        (marker-buffer (evil-test-marker-buffer-from-string
                        string
                        point-start point-end
                        visual-start visual-end))
        before-point after-point string selection)
    (unwind-protect
        (with-current-buffer marker-buffer
          (setq string (buffer-string))
          (when evil-test-point
            (setq before-point (buffer-substring (point-min) evil-test-point)
                  after-point (buffer-substring evil-test-point (point-max))))
          (when (and evil-test-visual-start evil-test-visual-end)
            (setq selection (buffer-substring
                             evil-test-visual-start evil-test-visual-end)))
          (with-current-buffer actual-buffer
            (if (or before-point after-point)
                (evil-test-text before-point after-point)
              ;; if the cursor isn't specified, just test the whole buffer
              (save-excursion
                (goto-char (point-min))
                (evil-test-text nil string #'bobp #'eobp)))
            (when selection
              (evil-test-selection selection))))
      (kill-buffer marker-buffer))))

(defun evil-test-buffer-from-string (string &optional
                                            state
                                            point-start point-end
                                            visual visual-start visual-end)
  "Create a new buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, then point is moved to that position.
If STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, then a Visual selection is created with those boundaries.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >.
STATE is the initial state; it defaults to `normal'.
VISUAL is the Visual selection: it defaults to `char'."
  (let ((type (evil-visual-type (or visual 'char)))
        (buffer (evil-test-marker-buffer-from-string
                 string point-start point-end
                 visual-start visual-end)))
    (with-current-buffer buffer
      (prog1 buffer
        (evil-change-state state)
        ;; let the buffer change its major mode without disabling Evil
        (add-hook 'after-change-major-mode-hook #'evil-initialize)
        (when (and (markerp evil-test-visual-start)
                   (markerp evil-test-visual-end))
          (evil-visual-select
           evil-test-visual-start evil-test-visual-end type)
          (when evil-test-point
            (goto-char evil-test-point)
            (evil-visual-refresh)
            (unless (and (= evil-visual-beginning
                            evil-test-visual-start)
                         (= evil-visual-end
                            evil-test-visual-end))
              (evil-visual-select
               evil-test-visual-start evil-test-visual-end type -1)
              (goto-char evil-test-point)
              (evil-visual-refresh))))
        (when (markerp evil-test-point)
          (goto-char evil-test-point))))))

(defun evil-test-marker-buffer-from-string (string &optional
                                                   point-start point-end
                                                   visual-start visual-end)
  "Create a new marker buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, that position is stored in the
buffer-local variable `evil-test-point'. Similarly,
if STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, those positions are stored in the variables
`evil-test-visual-beginning' and `evil-test-visual-end'.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >."
  (let ((string (or string ""))
        (point-start (regexp-quote
                      (if (characterp point-start)
                          (string point-start)
                        (or point-start "["))))
        (point-end (regexp-quote
                    (if (characterp point-end)
                        (string point-end)
                      (or point-end "]"))))
        (visual-start (regexp-quote
                       (if (characterp visual-start)
                           (string visual-start)
                         (or visual-start "<"))))
        (visual-end (regexp-quote
                     (if (characterp visual-end)
                         (string visual-end)
                       (or visual-end ">")))))
    (with-current-buffer (generate-new-buffer " *test*")
      (prog1 (current-buffer)
        (save-excursion
          (insert string))
        (save-excursion
          (when (> (length point-start) 0)
            (if (> (length point-end) 0)
                (when (re-search-forward
                       (format "\\(%s\\)[^%s]?\\(%s\\)"
                               point-start point-end point-end) nil t)
                  (goto-char (match-beginning 0))
                  (delete-region (match-beginning 2) (match-end 2))
                  (delete-region (match-beginning 1) (match-end 1))
                  (setq evil-test-point
                        (move-marker (make-marker) (point))))
              (when (re-search-forward point-start nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0))
                (setq evil-test-point
                      (move-marker (make-marker) (point)))))))
        (save-excursion
          (when (and (> (length visual-start) 0)
                     (> (length visual-end) 0))
            (when (re-search-forward visual-start nil t)
              (goto-char (match-beginning 0))
              (delete-region (match-beginning 0) (match-end 0))
              (setq evil-test-visual-start
                    (move-marker (make-marker) (point))))
            (when (re-search-forward visual-end nil t)
              (goto-char (match-beginning 0))
              (delete-region (match-beginning 0) (match-end 0))
              (setq evil-test-visual-end
                    (move-marker (make-marker) (point))))))))))

(defun evil-test-text (before after &optional before-predicate after-predicate)
  "Verify the text around point.
BEFORE is the expected text before point, and AFTER is
the text after point. BEFORE-PREDICATE is a predicate function
to execute at the beginning of the text, and AFTER-PREDICATE
is executed at the end."
  (when before
    (if (functionp before)
        (setq before-predicate before
              before nil)
      (should (string= (buffer-substring
                        (max (point-min) (- (point) (length before)))
                        (point))
                       before))))
  (when after
    (if (functionp after)
        (setq after-predicate after
              after nil)
      (should (string= (buffer-substring
                        (point)
                        (min (point-max) (+ (point) (length after))))
                       after))))
  (when before-predicate
    (ert-info ((format "Expect `%s' at the beginning" before-predicate))
      (save-excursion
        (backward-char (length before))
        (should (funcall before-predicate)))))
  (when after-predicate
    (ert-info ((format "Expect `%s' at the end" after-predicate))
      (save-excursion
        (forward-char (length after))
        (should (funcall after-predicate))))))

(defmacro evil-test-selection (string &optional end-string
                                      before-predicate after-predicate)
  "Verify that the Visual selection contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (or evil-visual-beginning (region-beginning)))
       (evil-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (or evil-visual-end (region-end)))
       (evil-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defmacro evil-test-region (string &optional end-string
                                   before-predicate after-predicate)
  "Verify that the region contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (region-beginning))
       (evil-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (region-end))
       (evil-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defmacro evil-test-overlay (overlay string &optional end-string
                                     before-predicate after-predicate)
  "Verify that OVERLAY contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (overlay-start ,overlay))
       (evil-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (overlay-end ,overlay))
       (evil-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defun evil-temp-filename ()
  "Return an appropriate temporary filename."
  (make-temp-name (expand-file-name "evil-test"
                                    temporary-file-directory)))

(defmacro evil-with-temp-file (file-var content &rest body)
  "Create a temp file with CONTENT and bind its name to FILE-VAR within BODY.
FILE-VAR must be a symbol which contains the name of the
temporary file within the macro body. CONTENT is either a string
to be used as the content of the temporary file or a form to be
executed with the temporary file's buffer as \(current-buffer),
see `with-temp-file'. BODY contains the forms to be executed
while the temporary file exists. The temporary file is deleted at
the end of the execution of BODY."
  (declare (indent 2)
           (debug (symbolp form body)))
  `(let ((,file-var (evil-temp-filename)))
     (with-temp-file ,file-var
       ,(if (stringp content)
            `(insert ,content)
          content))
     ,@body
     (delete-file ,file-var)))

(defun evil-test-file-contents (name contents)
  "Ensure that the contents of file with NAME equal CONTENTS."
  (with-temp-buffer
    (insert-file-contents name)
    (should (string= (buffer-string)
                     contents))))
