
.PHONY: all
all: gapc

ifeq ($(findstring $(MAKECMDGOALS),clean clean_all),)

testdata/config-tests/config.finished: config.mf
	$(MAKE) -C testdata/config-tests -f config.mf clean
	$(MAKE) -C testdata/config-tests -f config.mf

endif

ifeq ($(wildcard config.mf), config.mf)
  $(info ################################################################################)
  $(info Using user supplied build config from config.mf)
  $(info ################################################################################)
  -include testdata/config-tests/config.finished
  include config.mf
else
  $(info ################################################################################)
  $(info ################################################################################)
  $(error Copy config-templates/generic.mf to ./config.mf and adjust settings!)
endif


all: librna/librna$(SO_SUFFIX) librna/librnafast$(SO_SUFFIX)

# GNU make include order/file list working order in the single threaded case:
# if several included files have to be generated, make starts with
# the _last_ one
# this order is undocumented -> may change -> don't rely on this
GEN_CXXFILES = src/lexer.cc src/parser.cc

CXXFILES = $(wildcard src/*.cc) \
	   $(GEN_CXXFILES) \
	   $(wildcard src/type/*.cc) \
	   $(wildcard src/expr/*.cc) \
           $(wildcard src/statement/*.cc) \
	   $(wildcard src/ambiguity_cfg_gen/*.cc) \
	   $(wildcard src/cfg/*.cc) \
	   $(wildcard src/util/*.cc) \
	   $(wildcard src/printer/*.cc) \
	   $(wildcard src/specialize_grammar/*.cc) \
	   $(wildcard src/outside/*.cc) \
	   $(UNITTEST_CXX) \
           $(MODTESTS_CXX) \
           $(MODOUTSIDETESTS_CXX) \
	   $(wildcard testdata/test/*.cc) \
	   rtlib/string.cc \

DEPS = $(CXXFILES:.cc=.d)
OFILES = $(CXXFILES:.cc=.o)

# since bison 2.5 location/position.hh are not generated when
# location_type is defined
src/location.hh src/position.hh: testdata/config-tests/config.finished

src/location.hh: testdata/config-tests/location.hh
	cp $< $@

src/position.hh: testdata/config-tests/position.hh
	cp $< $@

# don't rely in which order make works on file lists
src/lexer.cc: src/parser.cc

src/parser.cc: testdata/config-tests/config.finished

# parser.o must be built first, since bison generates needed header files
src/parser.hh src/stack.hh: src/parser.cc

src/driver.o: src/parser.hh

$(filter-out src/parser.o src/lexer.o,$(OFILES)): src/location.hh src/position.hh src/stack.hh


include makefiles/deps.mf

ifdef NO_DEPS

# Brute force for compilers which do not support dependency
# generation, e.g. Sun CC
# for others the above location.hh ...: parser.y line is enough
$(filter-out src/parser.o src/lexer.o,$(OFILES)): src/parser.o

endif

EXEC_OBJ = $(filter testdata/modtest/%.o testdata/test/%.o ,$(OFILES)) src/gapc.o rtlib/string.o \
	   $(UNITTEST_CXX:.cc=.o)
MAIN_OBJ = $(filter-out $(EXEC_OBJ),$(OFILES))

LIBRNA_SRC = librna/rnalib.c librna/ViennaRNA/params/default.c librna/ViennaRNA/params/io.c librna/ViennaRNA/model.c librna/ViennaRNA/utils/utils.c librna/ViennaRNA/io/io_utils.c librna/ViennaRNA/params/params.c librna/ViennaRNA/alphabet.c librna/ViennaRNA/utils/string_utils.c
LIBRNA_OBJ = $(LIBRNA_SRC:.c=.o)
LIBRNA_PIO = $(LIBRNA_SRC:.c=.pio)

TEMP = $(GEN_CXXFILES) $(OFILES) $(DEPS)\
       $(TEST_TEMP) \
       src/parser.output src/parser.hh src/stack.hh src/location.hh \
       src/position.hh \
       gapc \
       $(MODTESTS) \
       $(MODOUTSIDETESTS) \
       stats testdata/modtest/stats.o \
       src/version.txt src/version.cc src/version.d src/version.o \
       testdata/config-tests/config.finished \
       src/prefix.cc src/prefix.d src/prefix.o \
       librna/librna$(SO_SUFFIX) \
       librna/librnafast$(SO_SUFFIX) librna/rnalib.fpio \
       librna/librna.a \
       $(LIBRNA_OBJ) \
       $(LIBRNA_PIO) \
       unittest/expr_lexer.o  unittest/expr_parser.o \
       unittest/expr_lexer.d  unittest/expr_parser.d \
       unittest/expr_parser.hh \
       unittest/expr_parser.output \
       unittest/location.hh unittest/position.hh

backtrack dep tab product listsizes indices code: \
  LDLIBS = $(BOOST_PROGRAM_OPTIONS_LIB)

%: $(MAIN_OBJ) testdata/modtest/%.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)

gapc: LDLIBS =  $(BOOST_PROGRAM_OPTIONS_LIB)

gapc: src/gapc.o $(MAIN_OBJ) src/version.o src/prefix.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)


# multi_rt_approx_rescore \

stats \
test_rt_tab \
multi_cyk multi_deps multi_indices multi_list_size multi_eliminate_lists_more multi_eliminate_lists multi_algebra multi_signature multi_rt_approx multi_rt_all multi_in_out multi_self_rec multi_calls multi_table_dim multi_max_filter multi_loops multi_ys parser outside_checksemantics outside_resolve_blocks outside_grammar outside_indices outside_loops: src/version.o src/prefix.o

backtrack: src/version.o

product typecheck tab indices listsizes dep: src/prefix.o src/version.o

range: LDLIBS =

range: testdata/modtest/range.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)


include makefiles/lexyaccxx.mf


.PHONY: clean
clean:
	rm -f $(TEMP)
	$(MAKE) -C testdata/config-tests/ -f config.mf clean
	rm -f testdata/unittest/expr_parser.{d,o,hh,output} testdata/unittest/expr_lexer.d

# test target: only triggers dependency tracking
.PHONY: depend
depend:
	touch dep


###############################################################################
# Shared libraries
###############################################################################

%.pio: %.c
	$(CC) -c -o $@ $(PIC_FLAGS) $(CPPFLAGS) $(CFLAGS) -I librna $<

%.fpio: %.c
	$(CC) -c -o $@ $(PIC_FLAGS) $(CPPFLAGS) $(CFLAGS) $(FAST_MATH) -I librna $<


librna/librna$(SO_SUFFIX): LDLIBS = -lm

librna/librna$(SO_SUFFIX): $(LIBRNA_PIO)
ifneq ($(INSTALL_SHARED_FLAG),)
	$(LD) $(SHARED_FLAGS) $(INSTALL_SHARED_FLAG)/librna$(SO_SUFFIX) $^ $(LDLIBS) -o $@
else
	$(LD) $(SHARED_FLAGS) $^ $(LDLIBS) -o $@
endif

librna/librnafast$(SO_SUFFIX): LDLIBS = -lm

librna/librnafast$(SO_SUFFIX): librna/rnalib.fpio $(filter-out librna/rnalib.pio, $(LIBRNA_PIO))
ifneq ($(INSTALL_SHARED_FLAG),)
	$(LD) $(SHARED_FLAGS) $(INSTALL_SHARED_FLAG)/librnafast$(SO_SUFFIX) $^ $(LDLIBS) -o $@
else
	$(LD) $(SHARED_FLAGS) $^ $(LDLIBS) -o $@
endif


librna/librna.a: $(LIBRNA_OBJ)
	$(AR) -r $@ $^


################################################################################
# version number generation
# In principle, version number shall be sourced as the date of the latest commit
# to the git repositoy.
# Unfortunately, for the automatic launchpad builds, the .git directory is not
# included to save space. Thus, for those situations, we must alternatively
# source the version number from the debian/changelog file
################################################################################
src/version.txt:
	@branch=$$(git rev-parse --abbrev-ref HEAD || echo "master"); \
	date=$$(git log -1 --date=format:"%Y.%m.%d" --format="%ad" 2>/dev/null || grep "20..\...\..." debian/changelog -m 1 -o); \
	if [ "$$branch" == "master" ]; then \
	  version="$$date"; \
	else \
		version="$$date-$$branch"; \
	fi; \
	echo "$$version" >$@;

src/version.cc: src/version.txt
	printf "#include \"version.hh\"\n\nnamespace gapc {\n  const char version_id[] = \"`cat $<`\";\n}\n" > $@

################################################################################
# PREFIX
################################################################################

src/prefix.cc: config.mf
	printf "#include \"prefix.hh\"\n\nnamespace gapc {\nconst char prefix[] = \"$(PREFIX)\";\nconst char systemsuffix[] = \"$(SYSTEM_SUFFIX)\";\n}\n" > $@


################################################################################
# Install
################################################################################

.PHONY: install

install: gapc librna/librna$(SO_SUFFIX)
	$(SHELL) makefiles/run.sh $(PREFIX)

install-librna: librna/librna$(SO_SUFFIX)
	$(SHELL) makefiles/run-librna.sh $(PREFIX)

################################################################################
# Tests
################################################################################

.PHONY: unittests paraltests modtests test test-unit test-mod test-paral \
  test-regress


# unit test files
UNITTEST_CXX = $(wildcard testdata/unittest/*.cc)

TEST_TEMP=$(UNIT_EXECS) testdata/fp_eq

UNIT_EXECS=$(UNITTEST_CXX:.cc=)

unittests: $(UNITTEST_CXX:.cc=)

$(UNIT_EXECS): LDLIBS= $(BOOST_UNIT_TEST_FRAMEWORK_LIB)

testdata/unittest/runtime: src/runtime.o

testdata/unittest/rtlib: rtlib/string.o

testdata/unittest/rna.o: CPPFLAGS_EXTRA=-Ilibrna

testdata/unittest/rna: librna/librna.a

testdata/unittest/rna: LDLIBS=$(BOOST_UNIT_TEST_FRAMEWORK_LIB) librna/librna.a

testdata/unittest/rna.d: CPPFLAGS_EXTRA=-Ilibrna

testdata/unittest/sequence.o unittest/sequence.d: CPPFLAGS_EXTRA=-Ilibrna -Irtlib

testdata/unittest/sample: LDLIBS=$(BOOST_UNIT_TEST_FRAMEWORK_LIB) \
                        $(GSL_LIBS)

testdata/unittest/sample testdata/unittest/sample.o testdata/unittest/sample.d: CPPFLAGS_EXTRA=-DUSE_GSL


testdata/unittest/tablegen: testdata/unittest/expr_parser.o testdata/unittest/expr_lexer.o $(filter-out src/lexer.o src/parser.o src/driver.o, $(MAIN_OBJ)) src/version.o src/prefix.o

testdata/unittest/tablegen.o: testdata/unittest/expr_parser.o

testdata/unittest/expr_lexer.o: testdata/unittest/expr_parser.o

testdata/unittest/tracks testdata/unittest/productive: $(MAIN_OBJ) src/prefix.o src/version.o

testdata/unittest/%: testdata/unittest/%.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)


# modtest

MODTESTS_CXX:= $(wildcard testdata/modtest/multi*.cc)
MODOUTSIDETESTS_CXX:= $(wildcard testdata/modtest/outside*.cc)

MODTESTS:=$(sort $(subst testdata/modtest/,,$(MODTESTS_CXX:.cc=)))
MODOUTSIDETESTS:=$(sort $(subst testdata/modtest/,,$(MODOUTSIDETESTS_CXX:.cc=)))
modtests: $(MODTESTS)
outsidetests: $(MODOUTSIDETESTS)

# paraltest

paraltests: gapc librna/librna.a testdata/fp_eq

paraltest/fp_eq: LDFLAGS=$(C_LDFLAGS)

paraltest/fp_eq: LDLIBS=-lm

test: test-unit test-mod test-paral test-regress test-ambiguity

test-unit: unittests
	cd testdata/unittest &&\
	  $(SHELL) run.sh $(subst testdata/unittest/,./,$(UNIT_EXECS))

test-mod: modtests
	cd testdata/modtest &&\
	$(SHELL) run.sh $(TRUTH_DIR)/Mod$(TRUTH_SUFFIX) $(MODTESTS)

test-mod_outside: outsidetests
	cd testdata/modtest &&\
	$(SHELL) run_outside.sh $(TRUTH_DIR)/Mod$(TRUTH_SUFFIX) $(MODOUTSIDETESTS)

gen-mod: modtests
	cd testdata/modtest &&\
	  $(SHELL) gen.sh $(TRUTH_DIR)/Mod$(TRUTH_SUFFIX) $(MODTESTS)

test-paral: paraltests
	cd testdata/paraltest &&\
	$(SHELL) run.sh

test-regress: gapc librna/librna.a testdata/fp_eq testdata/regresstest/rand testdata/test/shapemfepfx/nowindow testdata/test/shapemfepfx/main testdata/test/shrepmfesample/main
	cd testdata/regresstest &&\
	$(SHELL) run.sh $(TRUTH_DIR)/Regress "" "$(SHELL)"

test-ambiguity: gapc
	cd testdata/ambiguitytest &&\
	$(SHELL) run.sh $(TRUTH_DIR)/Ambiguity "" "$(SHELL)"

testdata/test/shapemfepfx/nowindow testdata/test/shapemfepfx/main:
	$(MAKE) -C testdata/test/shapemfepfx all

testdata/test/shrepmfesample/main:
	$(MAKE) -C testdata/test/shrepmfesample all
