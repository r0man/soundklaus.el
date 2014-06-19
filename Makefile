
all: package

.PHONY: clean test

.cask:
	@cask install

clean:
	@rm -rf dist
	@cask clean-elc

compile: .cask
	@cask exec emacs --batch --eval="(byte-compile-file \"soundklaus.el\")" -q

lint: .cask
	@cask exec emacs soundklaus.el --batch --eval="(elint-current-buffer)" -q

distclean: clean
	@rm -rf .cask

package: test
	@cask package

test: .cask
	@cask exec ert-runner
