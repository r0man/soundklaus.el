
all: package

.PHONY: clean test

.cask:
	@cask install

clean:
	@rm -rf dist
	@cask clean-elc

lint: .cask
	@cask exec emacs soundklaus.el --batch --eval="(elint-current-buffer)"

distclean: clean
	@rm -rf .cask

package: test
	@cask package

test: .cask
	@cask exec ert-runner
