PWD      =`pwd`
SRC_DIR  =src
TEST_DIR =src
ATP      ?=online-atps --atp=metis
AGDA_DIR ?=${PWD}/lib/.agda

.PHONY : default
default : install-bin


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
	@echo "=== Updating repository of problems Prop-Pack (test/prop-pack) ===="
	@echo "==================================================================="
	git submodule update --init test/prop-pack

.PHONY: agda-stdlib
agda-stdlib:
	@echo "==================================================================="
	@echo "===== Downloading Agda standard library v0.13 (Agda 2.5.2) ========"
	@echo "==================================================================="
	@if [ ! -d lib/agda-stdlib ] ; \
	 then \
		git clone \
			-b 'v0.13' \
			--single-branch \
			https://github.com/agda/agda-stdlib.git \
			lib/agda-stdlib; \
	 else \
		 echo "[!] agda-stdlib directory already exists"; \
	 fi;


.PHONY : agda-libraries
agda-libraries:
	@echo "==================================================================="
	@echo "===== Downloading libraries: Agda-Prop and Agda-Metis (./lib) ====="
	@echo "==================================================================="
	git submodule update --init lib/agda-prop
	git submodule update --init lib/agda-metis


.PHONY: install-libraries
install-libraries: agda-stdlib agda-libraries
	@echo "==================================================================="
	@echo "====== Installing libraries: Agda-Prop and Agda-Metis (./lib) ====="
	@echo "==================================================================="

	@mkdir -p lib/.agda

	@> lib/.agda/libraries
	@echo "${PWD}/lib/agda-stdlib/standard-library.agda-lib" \
		>> ${PWD}/lib/.agda/libraries
	@echo "${PWD}/lib/agda-prop/agda-prop.agda-lib" \
		>> ${PWD}/lib/.agda/libraries
	@echo "${PWD}/lib/agda-metis/agda-metis.agda-lib" \
		>> ${PWD}/lib/.agda/libraries
	@echo "${PWD}/test/test.agda-lib" \
		>> ${PWD}/lib/.agda/libraries

	@> lib/.agda/defaults
	@echo "standard-library" >> lib/.agda/defaults
	@echo "test" >> lib/.agda/defaults

	@echo "[!] To complete the installation, please set the AGDA_DIR variable."
	@echo "    $$ export AGDA_DIR=${PWD}/lib/.agda"

.PHONY : install-bin
install-bin :
	@echo "==================================================================="
	@echo "================ Installing Athena v0.1 ==========================="
	@echo "==================================================================="
	cabal install --disable-documentation -v0 --jobs=1 -g --ghc

.PHONY: install
install :
	@make install-bin
	@make install-libraries
	@echo "[!] You can test Athena with basic problems:"
	@echo "    $$ make basic"
	@echo "    or with a bigger list of problems:"
	@echo "    $$ make check"


.PHONY : problems
problems : prop-pack
	@echo "==================================================================="
	@echo "================== Generating TSTP files of proofs ================"
	@echo "==================================================================="
	make --directory test/prop-pack solutions

.PHONY : reconstruct
reconstruct : install-bin problems
	@Echo "==================================================================="
	@echo "====================== Generating Agda files ======================"
	@echo "==================================================================="
	@find test/prop-pack/problems \
		-type f \
		-name "*.tstp" \
		-print \
		-exec sh -c "athena {}" \;;

.PHONY : check
check : reconstruct install-libraries
	@echo "==================================================================="
	@echo "================== Type-checking Agda files ======================="
	@echo "==================================================================="
	@find test/prop-pack/problems/basic \
			-type f \
			-name "*.agda" \
			-print \
			-exec sh -c "agda {} --verbose=0" \;;
	@find test/prop-pack/problems/conjunction \
			-type f \
			-name "*.agda" \
			-print \
			-exec sh -c "agda {} --verbose=0" \;;

	@echo "test/prop-pack/problems/implication/impl-1.agda"
	agda test/prop-pack/problems/implication/impl-1.agda --verbose=0

	@echo "test/prop-pack/problems/negation/neg-1.agda"
	agda test/prop-pack/problems/negation/neg-1.agda --verbose=0

	@echo "test/prop-pack/problems/negation/neg-2.agda"
	agda test/prop-pack/problems/negation/neg-2.agda --verbose=0

	@echo "test/prop-pack/problems/negation/neg-4.agda"
	agda test/prop-pack/problems/negation/neg-4.agda --verbose=0

	@echo "test/prop-pack/problems/negation/neg-10.agda"
	agda test/prop-pack/problems/negation/neg-10.agda --verbose=0

	@echo "test/prop-pack/problems/negation/neg-15.agda"
	agda test/prop-pack/problems/negation/neg-15.agda --verbose=0

	@echo "test/prop-pack/problems/negation/neg-16.agda"
	agda test/prop-pack/problems/negation/neg-16.agda --verbose=0

	@echo "test/prop-pack/problems/negation/neg-30.agda"
	agda test/prop-pack/problems/negation/neg-30.agda --verbose=0

	@echo "test/prop-pack/problems/negation/neg-31.agda"
	agda test/prop-pack/problems/negation/neg-31.agda --verbose=0


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
	@find test/prop-pack/problems/basic \
			-type f \
			-name "*.agda" \
			-print \
			-exec sh -c "agda {} --verbose=0 ; echo '...'" \;;
