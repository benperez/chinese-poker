.PHONY: setup
setup:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests


.PHONY: test
test:
	cabal exec -- runhaskell -isrc -itest test/Spec.hs


.PHONY: repl
repl:
	ghci -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d
