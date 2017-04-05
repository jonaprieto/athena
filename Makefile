SRC_DIR  =src
TEST_DIR =src
ATP      ?=online-atps --atp=metis

.PHONY : default
default : install-bin

.PHONY : install-bin
install-bin :
	cabal install --disable-documentation -v2 --jobs=1 -g --ghc

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

.PHONY : TAGS
TAGS :
	fast-tags -e -R src/

.PHONY : clean
clean :
	rm -f ${SRC_DIR}/TSTP/Lexer.hs
	rm -f ${SRC_DIR}/TSTP/Lexer.hi
	rm -f ${SRC_DIR}/TSTP/Lexer.o
	rm -f ${SRC_DIR}/TSTP/Parser.hs
	find ${SRC_DIR} -regex ".*\(\.hi\|\.o\|\.agdai\)$$" -delete
	find ${SRC_DIR} -name "cnf*" -delete
	find . -name "cnf*" -delete
	find . -name "saturation*" -delete
	find . -name "*.tstp" -delete
	find . -name "*.agda" -delete
	rm -rf dist
	if [ -a test/prop/Makefile ] ; \
	then \
		make --directory test/prop-pack clean ; \
	fi;



.PHONY : prop-pack
prop-pack :
	@echo "==================================================================="
	@echo "============ Updating repository of problems Prop-Pack ============"
	@echo "==================================================================="
	git submodule update --init test/prop-pack

.PHONY : problems
problems : prop-pack
	@echo "==================================================================="
	@echo "================== Generating TSTP files of proofs ================"
	@echo "==================================================================="
	make --directory test/prop-pack solutions

.PHONY : reconstruct
reconstruct : install-bin problems
	@echo "==================================================================="
	@echo "====================== Generating Agda files ======================"
	@echo "==================================================================="
	@find test/prop-pack/problems \
		-type f \
		-name "*.tstp" \
		-print \
		-exec sh -c "athena {}" \;;

.PHONY : check
check : reconstruct
	@echo "==================================================================="
	@echo "================== Type-checking Agda files ======================="
	@echo "==================================================================="
	@cd test/prop-pack/problems/basic && \
		agda basic-1.agda --verbose=0 && echo "basic-1.agda" && \
		agda basic-2.agda --verbose=0 && echo "basic-2.agda" && \
		agda basic-3.agda --verbose=0 && echo "basic-3.agda" && \
		agda basic-4.agda --verbose=0 && echo "basic-4.agda"
	@cd test/prop-pack/problems/conjunction && \
		agda conj-1.agda --verbose=0 && echo "conj-1.agda"


.PHONY : basic
basic : install-bin prop-pack
	@echo "==================================================================="
	@echo "======= Generating TSTP files of test/prop-pack/basic files ======="
	@echo "==================================================================="
	@make --directory test/prop-pack basic
	@echo "==================================================================="
	@echo "========= Generating Agda files from test/prop-pack/basic ========="
	@echo "==================================================================="
	@find test/prop-pack/problems/basic \
		-type f \
		-name "*.tstp" \
		-print \
		-exec sh -c "athena {}" \;;
	@echo "==================================================================="
	@echo "========= Type-checking test/prop-pack/basic Agda files ==========="
	@echo "==================================================================="
	@cd test/prop-pack/problems/basic && \
		find . \
			-type f \
			-name "*.agda" \
			-print \
			-exec sh -c "agda {} --verbose=0" \;;
