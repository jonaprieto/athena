SHELL    :=/bin/bash
PWD      :=$(realpath .)
SRC_DIR  =src
TEST_DIR =test

ATHENA   =athena
AGDA     =agda

.PHONY : default
default: install

# -----------------------------------------------------------------------------
# Naive way to alert the user about the Agda version or GHC version.
# -----------------------------------------------------------------------------

AGDA_VERSION := $(shell agda --version 2>/dev/null)
TESTED_AGDA_VERSION1 =Agda version 2.5.3

agdaversion:

ifneq ($(AGDA_VERSION),$(TESTED_AGDA_VERSION1))
	@echo "==================================================================="
	@echo "===================  WARNING: AGDA VERSION! ======================="
	@echo "==================================================================="
	@echo "[!] ATHENA was tested with:"
	@echo "    * ${TESTED_AGDA_VERSION1}"
	@echo "    Your system has a different version:"
	@echo "    * ${AGDA_VERSION}"
endif

GHC_VERSION := $(shell ghc --version 2>/dev/null)
TESTED_GHC_VERSION1 :=The Glorious Glasgow Haskell Compilation System, version 8.0.2
TESTED_GHC_VERSION2 :=The Glorious Glasgow Haskell Compilation System, version 8.2.1

ghcversion:

ifneq ($(GHC_VERSION),$(TESTED_GHC_VERSION1))
ifneq ($(GHC_VERSION),$(TESTED_GHC_VERSION2))
	@echo "==================================================================="
	@echo "====================  WARNING: GHC VERSION! ======================="
	@echo "==================================================================="
	@echo "[!] ATHENA was tested with:"
	@echo "    * ${TESTED_GHC_VERSION1}"
	@echo "    * ${TESTED_GHC_VERSION2}"
	@echo "    Your system has a different version:"
	@echo "    * ${GHC_VERSION}"
endif
endif

# -----------------------------------------------------------------------------
# Targets and variables to generate test problems and type-checking.
# -----------------------------------------------------------------------------

ATP             ?=online-atps --atp=metis
ATHENA_LIB      =$(addprefix $(PWD), /lib)
ATHENA_AGDA_LIB =$(addprefix $(ATHENA_LIB),/.agda)

TIME_BIN        := $(shell which time)
TIMELIMIT       =timeout 1m
# timelimit -T60 -t60 -S9
SEP='-------------------------------------------------------------------'


ifdef MSVC     # Avoid the MingW/Cygwin sections
		uname_S := Windows
else                          # If uname not available => 'not'
		uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
endif

# https://stackoverflow.com/questions/3466166/how-to-check-if-running-in-cygwin-mac-or-linux

ifeq ($(uname_S),OSF1)
		TIMELIMIT =timelimit -T60 -t60 -S9
		TIME_BIN  := $(shell which gtime) # install gnu-time
endif
ifeq ($(uname_S),Darwin)
		TIMELIMIT =timelimit -T60 -t60 -S9
		TIME_BIN  := $(shell which gtime)
endif
ifeq ($(uname_S),Linux)
		TIMELIMIT =timeout 1m
endif
ifeq ($(uname_S),GNU/kFreeBSD)
		TIMELIMIT =timelimit -T60 -t60 -S9
		TIME_BIN  := $(shell which gtime)
endif
ifeq ($(uname_S),UnixWare)
		TIMELIMIT =timeout 1m
endif


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
PMETIS    =$(addprefix $(PROBLEMS),/prop-metis)

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

TPTP_PMETIS := $(wildcard $(PMETIS)/*.tptp)
TSTP_PMETIS := $(addprefix $(PMETIS)/,$(notdir $(TPTP_PMETIS:.tptp=.tstp)))
AGDA_PMETIS := $(addprefix $(PMETIS)/,$(notdir $(TSTP_PMETIS:.tstp=.agda)))


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

$(PMETIS)/%.tstp: $(PMETIS)/%.tptp
	@echo $@
	@${ATP} $< > $@

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

$(PMETIS)/%.agda: $(PMETIS)/%.tstp
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

.PHONY : paper
paper :
	make --directory paper pdf
	@echo "File: paper/main.pdf"

.PHONY : slides
slides :
	make --directory slides pdf
	@echo "File: slides/Jonathan-Proof-Reconstruction.pdf"

.PHONY : clean
clean :
	@echo "==================================================================="
	@echo "======================== Cleaning routines ========================"
	@echo "==================================================================="
	@echo "Cleaning Parser's files."
	@echo ${SEP}
	rm -f ${SRC_DIR}/TSTP/Lexer.hs
	rm -f ${SRC_DIR}/TSTP/Lexer.hi
	rm -f ${SRC_DIR}/TSTP/Lexer.o
	rm -f ${SRC_DIR}/TSTP/Parser.hs
	@echo
	@echo "Cleaning Agda and Haskell auxiliar files."
	@echo ${SEP}
	rm -Rf bin/
	find ${SRC_DIR} -regex '.*\(\.hi\|\.o\|\.agdai\)$$' -delete
	find ${SRC_DIR} -name 'cnf*' -delete
	find . -name 'cnf*' -delete
	find . -name 'saturation*' -delete
	find . -name '*.tstp' -delete
	find ${TEST_DIR} -name '*.agdai' -delete
	find . \
		\( -path './lib' \) \
		-name '*.agda' -delete
	rm -rf dist
	rm -rf lib/.agda
	@echo
	@echo 'Cleaning Prop-Pack test problems.'
	@echo ${SEP}
	make --directory paper/ clean
	make --directory slides/ clean
	make --directory test/prop-pack clean

.PHONY : agda
agda :
	@echo "==================================================================="
	@echo "===================== Installing Agda v2.5.3  ====================="
	@echo "==================================================================="
	@cabal install agda-2.5.3

.PHONY : online-atps
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

.PHONY : agda-stdlib
agda-stdlib:
	@if [ ! -d lib/agda-stdlib ] ; \
	 then \
	 echo "===================================================================";\
	 echo "=========== Downloading Agda Standard Library v0.14    ============";\
	 echo "===================================================================";\
	 git config --global advice.detachedHead false && \
	 git clone -q --progress \
			-b 'v0.14' \
			--single-branch \
			https://github.com/agda/agda-stdlib.git \
			lib/agda-stdlib; \
	 echo "Installed agda-stdlib v0.14  in ${ATHENA_LIB}/agda-stdlib"; \
	 else \
		 echo "[!] agda-stdlib directory already exists"; \
	 fi;

.PHONY : submodules
submodules :
	@echo "==================================================================="
	@echo "============ Pulling lastest version in the submodules ============"
	@echo "==================================================================="
	git submodule foreach git pull origin master

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
	@echo
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
	@echo "${PWD}/notes/notes.agda-lib" \
		>> ${ATHENA_AGDA_LIB}/libraries
	@> lib/.agda/defaults
	@echo "standard-library" >> lib/.agda/defaults
	@echo "test" >> lib/.agda/defaults
	@echo "notes" >> lib/.agda/defaults
	@echo "Libraries"
	@echo "---------"
	@echo
	@echo "  $$ cat ${ATHENA_AGDA_LIB}/libraries"
	@echo
	@cat  ${ATHENA_AGDA_LIB}/libraries
	@echo
	@echo "Defaults"
	@echo "---------"
	@echo
	@echo "  $$ cat ${ATHENA_AGDA_LIB}/defaults"
	@echo
	@cat  ${ATHENA_AGDA_LIB}/defaults
	@echo
	@echo "[!] To complete the installation, please set the AGDA_DIR variable,"
	@echo "you can do that executing the following command:"
	@echo 
	@echo "  $$ export AGDA_DIR=${ATHENA_AGDA_LIB}"
	@echo

.PHONY : install
install : ghcversion
	@echo "==================================================================="
	@echo "===================== Installing Athena v0.1 ======================"
	@echo "==================================================================="
	cabal install --disable-documentation -g --ghc

.PHONY : prop-pack
prop-pack :
	@echo "==================================================================="
	@echo "==== Downloading problems from github.com/jonaprieto/prop-pack ===="
	@echo "==================================================================="
	git submodule update --init test/prop-pack

.PHONY: msg-tstp
msg-tstp :
	@echo "==================================================================="
	@echo "================= Generating TSTP files of proofs ================="
	@echo "==================================================================="
	@echo
	@echo "[!] To use Metis locally instead of using Metis from OnlineATPs, "
	@echo "please set ATP variable in your environment:"
	@echo
	@echo "  $$ export ATP=\"metis --show proof\""
	@echo
	@echo "If you don't have Metis anyway, you can install OnlineATPs:"
	@echo
	@echo "  $$ make online-atps"
	@echo "  $$ export ATP=\"online-atps --atp=metis\""
	@echo
	@echo ${SEP}


.PHONY : problems
problems : prop-pack \
					 msg-tstp \
					 $(TSTP_BASIC) \
					 $(TSTP_CONJ)  \
					 $(TSTP_DISJ)  \
					 $(TSTP_IMPL)  \
					 $(TSTP_BICOND) \
					 $(TSTP_NEG)  \
					 $(TSTP_PMETIS)

.PHONY : reconstruct
reconstruct : install problems
	@echo "==================================================================="
	@echo "============== Generating Agda files of TSTP proofs ==============="
	@echo "==================================================================="
	@echo
	@echo "If you want to generate an Agda file from the tests,"
	@echo "you can execute the following command in your shell:"
	@echo
	@echo "  $$ athena TSTPFileGeneratedByMETIS.tpsp"
	@echo
	@echo ${SEP}
	@find test/prop-pack/problems \
		-type f \
		-name "*.tstp" \
		-exec sh -c "athena --debug {} && echo ${SEP}" {} \;;


AGDACALL ="${TIMELIMIT} \
	${TIME_BIN} -f \"user = %U, system = %S, elapsed = %E, mem=%K, cpu=%P\" \
	${AGDA} $$agdaFile --verbose=0 --library=test"

.PHONY : check
check : export AGDA_DIR := $(ATHENA_AGDA_LIB)
check : reconstruct  \
				agdaversion  \
				install-libraries  \
				$(AGDA_BASIC) \
				$(AGDA_CONJ)  \
				$(AGDA_DISJ)  \
				$(AGDA_IMPL)  \
				$(AGDA_BICOND)  \
				$(AGDA_NEG)   \
				$(AGDA_PMETIS)

	@echo "==================================================================="
	@echo "================== Type-checking Agda files ======================="
	@echo "==================================================================="
	@echo
	@echo "[!] AGDA_DIR=${AGDA_DIR}"
	@echo
	@echo "If you want to type-check an isolote Agda file from the tests,"
	@echo "you can execute the following command in your shell:"
	@echo
	@echo "  $$ pwd"
	@echo "  $(PWD)"
	@echo "  $$ export AGDA_DIR=${ATHENA_AGDA_LIB}"
	@echo "  $$ agda --library=test AgdaFileGeneratedByAthena"
	@echo
	@echo ${SEP}

	@for agdaFile in `find ${TEST_DIR} \
			-type f -name "*.agda" \
			-not -path "*prop-21.agda" | sort`; do \
		echo $$agdaFile; \
		sh -c $(AGDACALL); \
		echo ${SEP}; \
	done
