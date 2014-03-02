GHCFLAGS=-Wall -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8
VERSION=0.1

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHSvogogo-$(VERSION).a dist/vogogo-$(VERSION).tar.gz

install: dist/build/libHSvogogo-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Vogogo.hs Vogogo/Account.hs Vogogo/Customer.hs Vogogo/Internal.hs Vogogo/Transaction.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/vogogo/index.html README

README: vogogo.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/vogogo/index.html: dist/setup-config Vogogo.hs Vogogo/Account.hs Vogogo/Customer.hs Vogogo/Internal.hs Vogogo/Transaction.hs
	cabal haddock --hyperlink-source

dist/setup-config: vogogo.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHSvogogo-$(VERSION).a: dist/setup-config Vogogo.hs Vogogo/Account.hs Vogogo/Customer.hs Vogogo/Internal.hs Vogogo/Transaction.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/vogogo-$(VERSION).tar.gz: README dist/setup-config Vogogo.hs Vogogo/Account.hs Vogogo/Customer.hs Vogogo/Internal.hs Vogogo/Transaction.hs
	cabal check
	cabal sdist
