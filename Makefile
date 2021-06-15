emacs ?= emacs
bemacs = $(emacs) -batch -l test/elpa.el
LIBS = -L evil -L evil/lib

update:
	$(emacs) -batch -l test/make-update.el && \
	[ ! -d evil ] && \
	git clone --depth 1 https://github.com/emacs-evil/evil.git

emacs:
	$(emacs) -Q $(LIBS) \
	--eval "(load-file \"evil/evil.el\")" \
	--eval "(load-file \"evil/lib/goto-chg.el\")" \
	--eval "(load-file \"evil-surround.el\")" \
	--eval "(evil-mode 1)" \
	--eval "(global-evil-surround-mode 1)"


compile: clean
	$(bemacs) -l test/make-compile.el

test:
	$(bemacs) -l test/make-test.el


clean:
	rm -f *.elc



.PHONY: update compile test clean
