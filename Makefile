SRC_DIR = src
ATP     = "online-atps --atp=metis"

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
	make clean
	make hlint
	make check-whitespace
	make haddock
	@echo "$@ succeeded!"

.ONESHELL :
.PHONY : problems
problems :
	@cd test/prop-pack
	@git submodule update --init --recursive
	@make --directory test/prop-pack solutions

.ONESHELL :
.PHONY : reconstruct
reconstruct :
	@cd test/prop-pack/problems/
	@echo "Generating Agda files"
	@echo "====================="
	@find . \
		-type f -name "*.tstp" \
		-print \
		-exec sh -c "athena {}" \;;


.ONESHELL :
.PHONY : check-proofs
check :
	@cd test/prop-pack/problems/
	@echo "Checking Agda files"
	@echo "==================="
	@find . \
		-type f -name "*.agda" \
		-print \
		-exec sh -c "agda {} --verbose=0" \;;

.ONESHELL :
.PHONY : basic
basic :
	@cd test/prop-pack
	@make --directory test/prop-pack basic
	@echo "Reconstructing test/prop-pack/problems/basic TSTP files"
	@echo "======================================================="
	@find problems/basic \
		-type f -name "*.tstp" \
		-print \
		-exec sh -c "athena {}" \;;
	@echo "Type-cheking test/prop-pack/problems/basic files"
	@echo "================================================"
	@find problems/basic \
	-type f -name "*.agda" \
	-print \
	-exec sh -c "agda {} --verbose=0" \;;
