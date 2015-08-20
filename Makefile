.PHONY: setup
setup:
	cabal sandbox init
	cabal install --jobs
