emacs ?= emacs
bemacs = $(emacs) -batch -l test/elpa.el
LIBS = -L evil -L evil/lib -l evil.el -l goto-chg.el -l undo-tree.el -L . -l evil-surround.el -L test -l evil-surround-test.el

update:
	$(emacs) -batch -l test/make-update.el && \
	[ ! -d evil ] && \
	git clone --depth 1 https://github.com/emacs-evil/evil.git

emacs:
	$(emacs) -Q $(LIBS) \
	--eval "(evil-mode 1)" \
	--eval "(global-evil-surround-mode 1)"

compile: clean
	$(bemacs) -l test/make-compile.el

test:
	$(bemacs) -l test/make-test.el


clean:
	rm -f *.elc



.PHONY: update compile test clean
