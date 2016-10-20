emacs ?= emacs
CASK ?= cask
BEMACS = $(emacs) -batch -l elpa/elpa.el
LOAD = -l evil-textobj-anyblock.el

cask:
	$(shell EMACS=$(emacs) $(CASK) --verbose --debug)

test:
	@echo "Using $(shell which $(emacs))..."
		$(BEMACS) -l evil-textobj-anyblock-tests.el $(LOAD) \

clean:
	rm -f *.elc

.PHONY: cask test clean
