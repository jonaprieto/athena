SRC_DIR=src/Athena

# Requires HLint >= 1.9.36 and run `cabal build` or `cabal install`
# before.
.PHONY : hlint
hlint :
	hlint --color=never Setup.hs
	hlint --color=never \
							--cpp-file=dist/build/autogen/cabal_macros.h \
							--cpp-include=src/ \
							src/
	@echo "$@ succeeded!"

.PHONY : haddock
haddock :
	cabal configure
	cabal haddock --executables \
								--haddock-option=--use-unicode \
								--hyperlink-source
	@echo "$@ succeeded!"

.PHONY : install-bin
install-bin :
	- cabal configure --enable-tests --enable-benchmarks -v2
	- cabal build
	- cabal install --disable-documentation
	- cabal sdist

.PHONY : install-fix-whitespace
install-fix-whitespace :
	cd src/fix-whitespace && cabal install

.PHONY : check-whitespace
check-whitespace :
	fix-whitespace --check

.PHONY : TODO
TODO :
	@find . -type d \( -path './.git' -o -path './dist' \) -prune -o -print \
	| xargs grep -I -s 'TODO' \
	| sort

.PHONY : clean
clean :
	@rm -f ${SRC_DIR}/TSTP/Lexer.hs
	@rm -f ${SRC_DIR}/TSTP/Parser.hs
	@find ${SRC_DIR} -regex ".*\(\.hi\|\.o\|\.agdai\)$$" -delete
	@rm -f cnf*
	@rm -f saturation.tptp
	@rm -rf dist


.PHONY : tests
tests :
	- make clean
	- make hlint
	- make check-whitespace
	- make haddock
	- @echo "$@ succeeded!"
