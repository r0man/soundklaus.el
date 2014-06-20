
all: package

.PHONY: clean test

.cask:
	@cask install

clean:
	@rm -rf dist
	@cask clean-elc


compile: .cask
	@cask exec emacs --batch --eval="(batch-byte-compile)" -Q soundklaus.el

checkdoc:
	@cask exec emacs --batch --eval="(checkdoc)" -Q  soundklaus.el

lint: .cask
	@cask exec emacs soundklaus.el --batch --eval="(elint-current-buffer)" -q

distclean: clean
	@rm -rf .cask

package: test checkdoc
	@cask package

test: .cask
	@cask exec ert-runner
