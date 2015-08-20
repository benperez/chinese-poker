.PHONY: setup
setup:
	cabal sandbox init
	cabal install --jobs


.PHONY: repl
repl:
	ghci -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d
