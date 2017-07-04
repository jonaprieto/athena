PWD      =$(realpath .)
SRC_DIR  =src
TEST_DIR =src

ATHENA   =athena
AGDA     =agda
ATP      ?=online-atps --atp=metis
ATHENA_LIB      =$(addprefix $(PWD), /lib)
ATHENA_AGDA_LIB =$(addprefix $(ATHENA_LIB),/.agda)
TIMELIMIT =timeout -T240 -t240 -S9
AGDACALL ="${TIMELIMIT} ${AGDA} {} --verbose=0 && \
  echo '-------------------------------------------------------------------' && echo"


# ============================================================================
# Propositional problems in TPTP format from
# http://github.com/jonaprieto/prop-pack

PROP_PACK =test/prop-pack
PROBLEMS  =$(addprefix $(PROP_PACK),/problems)

BASIC     =$(addprefix $(PROBLEMS),/basic)
BICOND    =$(addprefix $(PROBLEMS),/biconditional)
CONJ      =$(addprefix $(PROBLEMS),/conjunction)
DISJ      =$(addprefix $(PROBLEMS),/disjunction)
IMPL      =$(addprefix $(PROBLEMS),/implication)
NEG       =$(addprefix $(PROBLEMS),/negation)

TPTP_BASIC := $(wildcard $(BASIC)/*.tptp)
TSTP_BASIC := $(addprefix $(BASIC)/,$(notdir $(TPTP_BASIC:.tptp=.tstp)))
AGDA_BASIC := $(addprefix $(BASIC)/,$(notdir $(TSTP_BASIC:.tstp=.agda)))

TPTP_CONJ := $(wildcard $(CONJ)/*.tptp)
TSTP_CONJ := $(addprefix $(CONJ)/,$(notdir $(TPTP_CONJ:.tptp=.tstp)))
AGDA_CONJ := $(addprefix $(CONJ)/,$(notdir $(TSTP_CONJ:.tstp=.agda)))

TPTP_DISJ := $(wildcard $(DISJ)/*.tptp)
TSTP_DISJ := $(addprefix $(DISJ)/,$(notdir $(TPTP_DISJ:.tptp=.tstp)))
AGDA_DISJ := $(addprefix $(DISJ)/,$(notdir $(TSTP_DISJ:.tstp=.agda)))

TPTP_IMPL := $(wildcard $(IMPL)/*.tptp)
TSTP_IMPL := $(addprefix $(IMPL)/,$(notdir $(TPTP_IMPL:.tptp=.tstp)))
AGDA_IMPL := $(addprefix $(IMPL)/,$(notdir $(TSTP_IMPL:.tstp=.agda)))

TPTP_BICOND := $(wildcard $(BICOND)/*.tptp)
TSTP_BICOND := $(addprefix $(BICOND)/,$(notdir $(TPTP_BICOND:.tptp=.tstp)))
AGDA_BICOND := $(addprefix $(BICOND)/,$(notdir $(TSTP_BICOND:.tstp=.agda)))

TPTP_NEG := $(wildcard $(NEG)/*.tptp)
TSTP_NEG := $(addprefix $(NEG)/,$(notdir $(TPTP_NEG:.tptp=.tstp)))
AGDA_NEG := $(addprefix $(NEG)/,$(notdir $(TSTP_NEG:.tstp=.agda)))

# ============================================================================

$(BASIC)/%.tstp: $(BASIC)/%.tptp
	@echo $@
	@${ATP} $< > $@

$(CONJ)/%.tstp: $(CONJ)/%.tptp
	@echo $@
	@${ATP} $< > $@

$(DISJ)/%.tstp: $(DISJ)/%.tptp
	@echo $@
	@${ATP} $< > $@

$(IMPL)/%.tstp: $(IMPL)/%.tptp
	@echo $@
	@${ATP} $< > $@

$(BICOND)/%.tstp: $(BICOND)/%.tptp
	@echo $@
	@${ATP} $< > $@

$(NEG)/%.tstp: $(NEG)/%.tptp
	@echo $@
	@${ATP} $< > $@

# ...

$(BASIC)/%.agda: $(BASIC)/%.tstp
	@echo $@
	@$(ATHENA) $<

$(CONJ)/%.agda: $(CONJ)/%.tstp
	@echo $@
	@$(ATHENA) $<

$(DISJ)/%.agda: $(DISJ)/%.tstp
	@echo $@
	@$(ATHENA) $<

$(NEG)/%.agda: $(NEG)/%.tstp
	@echo $@
	@$(ATHENA) $<

$(BICOND)/%.agda: $(BICOND)/%.tstp
	@echo $@
	@$(ATHENA) $<

$(IMPL)/%.agda: $(IMPL)/%.tstp
	@echo $@
	@$(ATHENA) $<

# ============================================================================

.PHONY : checklines
checklines :
	@grep '.\{80,\}' \
		--exclude-dir=src/Athena/TSTP \
		--exclude-dir=src/fix-whitespace \
		-l --recursive src; \
		status=$$?; \
		if [ $$status = 0 ] ; \
		then echo "Lines were found with more than 80 characters!"; \
		else echo "Succeed!"; \
		fi

.PHONY : hlint
hlint :
	hlint --color=never Setup.hs
	hlint --color=never \
							--cpp-file=dist/build/autogen/cabal_macros.h \
							--cpp-include=src/ \
							--ignore=src/Athena/TSTP/Lexer.hs \
							--ignore=src/Athena/TSTP/Parser.hs \
							--ignore=src/fix-whitespace/FixWhitespace.hs \
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
	cd src && fix-whitespace

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
	rm -Rf bin/
	find ${SRC_DIR} -regex '.*\(\.hi\|\.o\|\.agdai\)$$' -delete
	find ${SRC_DIR} -name 'cnf*' -delete
	find . -name 'cnf*' -delete
	find . -name 'saturation*' -delete
	find . -name '*.tstp' -delete
	find . \
		\( -path './lib' \) \
		-name '*.agda' -delete
	rm -rf dist
	rm -rf lib/.agda
	if [ -a test/prop/Makefile ] ; \
	then \
		make --directory test/prop-pack clean ; \
	fi;

online-atps:
	 @echo "==================================================================="
	 @echo "================= Installing Online-ATPs v0.1.1 ==================="
	 @echo "==================================================================="
	 @rm -Rf bin
	 @mkdir -p bin
	 @git config --global advice.detachedHead false && \
		git clone -q --progress \
		-b 'v0.1.1' \
		--single-branch \
		https://github.com/jonaprieto/online-atps.git \
		bin/online-atps
	 @cabal update
	 @cd bin/online-atps && cabal install
	 @rm -Rf bin

agda-stdlib:
	@if [ ! -d lib/agda-stdlib ] ; \
	 then \
	 echo "===================================================================";\
	 echo "===== Downloading Agda standard library v0.13 (Agda 2.5.2) ========";\
	 echo "===================================================================";\
	 git config --global advice.detachedHead false && \
	 git clone -q --progress \
			-b 'v0.13' \
			--single-branch \
			https://github.com/agda/agda-stdlib.git \
			lib/agda-stdlib; \
	 echo "Installed agda-stdlib v0.13 in ${ATHENA_LIB}/agda-stdlib"; \
	 else \
		 echo "[!] agda-stdlib directory already exists"; \
	 fi;

.PHONY : agda-libraries
agda-libraries:
	@echo "==================================================================="
	@echo "======== Downloading libraries: Agda-Prop and Agda-Metis =========="
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
	@echo "${ATHENA_LIB}/agda-stdlib/standard-library.agda-lib" \
		>> ${ATHENA_AGDA_LIB}/libraries
	@echo "${ATHENA_LIB}/agda-prop/agda-prop.agda-lib" \
		>> ${ATHENA_AGDA_LIB}/libraries
	@echo "${ATHENA_LIB}/agda-metis/agda-metis.agda-lib" \
		>> ${ATHENA_AGDA_LIB}/libraries
	@echo "${PWD}/test/test.agda-lib" \
		>> ${ATHENA_AGDA_LIB}/libraries

	@> lib/.agda/defaults
	@echo "standard-library" >> lib/.agda/defaults
	@echo "test" >> lib/.agda/defaults
	@echo "Libraries in ${ATHENA_AGDA_LIB}/libraries:"
	@cat  ${ATHENA_AGDA_LIB}/libraries
	@echo "[!] To complete the installation, please set the AGDA_DIR variable:"
	@echo "    $$ export AGDA_DIR=${ATHENA_AGDA_LIB}"

.PHONY : install-bin
install-bin :
	@echo "==================================================================="
	@echo "================ Installing Athena v0.1 ==========================="
	@echo "==================================================================="
	cabal install --disable-documentation -v0 --jobs=1 -g --ghc

.PHONY: install
install :
	@make reconstruct
	@make check

.PHONY : prop-pack
prop-pack :
	@echo "==================================================================="
	@echo "==== Downloading problems from github.com/jonaprieto/prop-pack ===="
	@echo "==================================================================="
	git submodule update --init test/prop-pack

.PHONY: msg-tstp
msg-tstp:
	@echo "==================================================================="
	@echo "=============== Generating TSTP files of proofs ==================="
	@echo "==================================================================="
	@echo "[!] To use Metis locally instead of using Metis from OnlineATPs, "
	@echo "    please set ATP variable in your environment:"
	@echo "    $$ export ATP=\"metis --show proof\""
	@echo "    If you don't have Metis anyway, you can install OnlineATPs:"
	@echo "    $$ make online-atps"
	@echo "    $$ export ATP=\"online-atps --atp=metis\""

.PHONY : problems
problems : prop-pack \
					 msg-tstp \
					 $(TSTP_BASIC) \
					 $(TSTP_CONJ)	\
					 $(TSTP_DISJ) \
					 $(TSTP_IMPL)	\
					 $(TSTP_BICOND) \
					 $(TSTP_NEG)

.PHONY : reconstruct
reconstruct : install-bin problems
	@echo "==================================================================="
	@echo "============== Generating Agda files of TSTP proofs ==============="
	@echo "==================================================================="
	@find test/prop-pack/problems \
		-type f \
		-name "*.tstp" \
		-print \
		-exec sh -c "athena {}" \;;

.PHONY : check
check : export AGDA_DIR := $(ATHENA_AGDA_LIB)
check : install-libraries \
				$(AGDA_BASIC) \
				$(AGDA_CONJ)	\
				$(AGDA_DISJ) \
				$(AGDA_IMPL)	\
				$(AGDA_BICOND) \
				$(AGDA_NEG)

	@echo "==================================================================="
	@echo "================== Type-checking Agda files ======================="
	@echo "==================================================================="
	@echo "[!] AGDA_DIR=${AGDA_DIR}"
	@find $(BASIC) \
					-type f \
					-name "*.agda" \
					-print \
					-exec sh -c $(AGDACALL) \;;

	@find $(CONJ) \
	 				-type f \
	 				-name "*.agda" \
	 				-print \
	 				-exec sh -c $(AGDACALL) \;;

	@find $(IMPL) \
				-type f \
				-name "*.agda" \
				-print \
				-exec sh -c $(AGDACALL) \;;

	@find $(DISJ) \
				-type f \
				-name "*.agda" \
				-print \
				-exec sh -c $(AGDACALL) \;;

	@find $(NEG) \
				-type f \
				-name "*.agda" \
				-print \
				-exec sh -c $(AGDACALL) \;;	

	@find $(BICOND) \
			-type f \
			-name "*.agda" \
			-print \
		  -exec sh -c $(AGDACALL) \;;

