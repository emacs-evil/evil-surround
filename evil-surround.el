;;; evil-surround.el --- emulate surround.vim in evil

;; Copyright (C) 2010 Tim Harper
;;
;; Author: Tim Harper <timcharper at gmail dat com>
;;      Please send bug reports to the mailing list (see below).
;; Created: August 11, 2011
;; Ported from vimpulse-surround.git
;; Version: 0.1pre1
;; Keywords: emulations, evil
;; Human-Keywords: vim, visual-mode, surround.vim
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;; Related: evil.el
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; `evil-surround' emulates surround.vim, a popular Vim plugin.
;;
;; The functionality is wrapped into a global minor mode, enabled by default.
;; 
;; (require 'evil-surround)
;; (evil-mode 1)
;;
;; The code requires a recent evil version. More information on evil
;; and how to get it can be found here:

;; http://gitorious.org/evil

;;; Code:
(require 'evil)

;; Evil-surround
(defgroup evil-surround nil
  "surround.vim for Emacs"
  :prefix "evil-surround-"
  :group 'evil)

(defcustom evil-surround-pairs
  '((")" . ("(" . ")"))
    ("(" . ("( " . " )"))
    ("]" . ("[" . "]"))
    ("[" . ("[ " . " ]"))
    ("}" . ("{" . "}"))
    ("{" . ("{ " . " }"))
    ("#" . ("#{" . "}"))
    ("t" . evil-surround-read-tag)
    ("<" . evil-surround-read-tag)
    (">" . ("<" . ">")))
  "Alist of surround items.
Each item is of the form (TRIGGER . (LEFT . RIGHT)), all strings.
Alternatively, a function can be put in place of (LEFT . RIGHT).
This only affects inserting pairs, not deleting or changing them."
  :group 'evil-surround
  :type '(repeat (cons (regexp :tag "Key")
                       (symbol :tag "Surround pair"))))

(defun evil-surround-char-to-pair (char)
  (let ((pair (or (assoc-default char evil-surround-pairs)
                  (cons char char))))
    (if (functionp pair)
        (funcall pair)
      pair)))

(defvar *evil-surrounding* nil
  "Internal variable set by `evil-surround-define-text-object'.
It triggers `evil-change'. Nothing to see here, move along.")

(defvar *evil-surround-start-size* nil)
(defvar *evil-surround-end-size* nil)

(defvar evil-surround-read-tag-keymap
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map ">" 'exit-minibuffer)
    map))

(defun evil-surround-read-tag ()
  (let* ((input (read-from-minibuffer "<" "" evil-surround-read-tag-keymap))
         (_ (string-match "\\([a-z0-9-]+\\)\\(.*?\\)[>]*$" input))
         (tag  (match-string 1 input))
         (rest (match-string 2 input)))
    (cons (format "<%s%s>" tag rest) (format "</%s>" tag))))

(evil-define-operator evil-Surround-region (beg end type register yank-handler)
  "Surround selection with input."
  (interactive (list evil-this-register (evil-yank-handler)))
  (let ((pair (evil-surround-char-to-pair
               (format "%c" (evil-read-key))))
        (o (make-overlay beg end)))
    (goto-char (overlay-start o))
    (insert (car pair))
    (indent-according-to-mode)
    (newline-and-indent)
    (goto-char (overlay-end o))
    (newline)
    (insert (cdr pair))
    (indent-according-to-mode)
    (goto-char (overlay-start o))
    (delete-overlay o)))

(evil-define-operator evil-surround-region (beg end type register yank-handler)
  "Surround region by the input-specified pair"
  (interactive (list evil-this-register (evil-yank-handler)))
  (if (equal type 'line)
      (evil-Surround-region beg end type register yank-handler)
    (let ((pair (evil-surround-char-to-pair
                 (format "%c" (evil-read-key))))
          (o (make-overlay beg end)))
      (goto-char (overlay-start o))
      (insert (car pair))
      (goto-char (overlay-end o))
      (insert (cdr pair))
      (goto-char (overlay-start o))
      (delete-overlay o))))

(defun evil-surround-zap-whitespace (direction boundary)
  (let ((pred (if (= direction 1)
		  'looking-at
		'looking-back)))
    (while (and (funcall pred "[ \t]") (not (= (point) boundary)))
      (delete-char direction)
      (when (= direction 1) (setq boundary (1- boundary))))))

(defvar evil-surround-mode-map (make-sparse-keymap))

(define-minor-mode evil-surround-mode
  "Emulate the surround.vim Vim plugin in Evil."
  t nil :global t :keymap evil-surround-mode-map)

(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-Surround-region)

(provide 'evil-surround)
;;; evil-surround.el ends here
