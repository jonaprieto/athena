SRC_DIR=src

# Requires HLint >= 1.9.36 and run `cabal build` or `cabal install`
# before.
.PHONY : hlint
hlint :
	hlint --color=never Setup.hs
	hlint --color=never \
							--cpp-file=dist/build/autogen/cabal_macros.h \
							--cpp-include=src/ \
							--ignore=src/Athena/TSTP/Lexer.hs \
							--ignore=src/Athena/TSTP/Parser.hs \
							${SRC_DIR}/
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

.PHONY : fix-whitespace
fix-whitespace :
	fix-whitespace

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
	@rm -f ${SRC_DIR}/TSTP/Lexer.hi
	@rm -f ${SRC_DIR}/TSTP/Lexer.o
	@rm -f ${SRC_DIR}/TSTP/Parser.hs
	@find ${SRC_DIR} -regex ".*\(\.hi\|\.o\|\.agdai\)$$" -delete
	@find ${SRC_DIR} -name "cnf*" -delete
	@find . -name "cnf*" -delete
	@find . -name "saturation*" -delete
	@find . -name "*.tstp" -delete
	@find . -name "*.agda" -delete
	@make --directory test/prop-pack clean
	@rm -rf dist


.PHONY : tests
tests :
	- make clean
	- make hlint
	- make check-whitespace
	- make haddock
	- @echo "$@ succeeded!"

.ONESHELL :
.PHONY : problems
problems :
	git submodule init
	git submodule update
	export ATP="online-atps --atp=metis"
	cat test/prop-pack/Makefile
	make --directory test/prop-pack solutions

.PHONY : update-problems
update-problems :
	git submodule update --remote
	make --directory test/prop-pack solutions

.PHONY : reconstruct
reconstruct :
	@echo "Generating Agda files"
	@echo "====================="
	@find test/prop-pack/problems \
		-type f -name "*.tstp" \
		-print \
		-exec sh -c "athena {}" \;;

.PHONY : check-proofs
check :
	@echo "Checking Agda files"
	@echo "==================="
	@cd test && find prop-pack/problems \
		-type f -name "*.agda" \
		-print \
		-exec sh -c "agda {} --verbose=0" \;;

.PHONY : test-basic
test-basic :
		@echo "Testing test/prop-pack/problems/basic"
		@echo "====================================="
		@cd test && find prop-pack/problems/basic \
		-type f -name "*.agda" \
		-print \
		-exec sh -c "agda {} --verbose=0" \;;
