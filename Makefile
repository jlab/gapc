
SO_SUFFIX = .so
AR = ar

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
GEN_CXXFILES = lexer.cc parser.cc

CXXFILES = statement/fn_call.cc expr/fn_call.cc tablegen.cc \
           table.cc yieldsize.cc tracks_visitor.cc \
	   para_decl.cc char_visitor.cc \
           statement/marker_decl.cc subopt_marker.cc \
           statement/hash_decl.cc \
	   statement/block_base.cc statement/while.cc \
           classify_visitor.cc kbacktrack.cc unused_visitor.cc \
           backtrack_base.cc subopt.cc codegen.cc backtrack.cc \
	   $(wildcard type/*.cc) \
	   $(wildcard expr/*.cc) \
           $(wildcard statement/*.cc) \
           mode.cc dep_analysis.cc loc.cc input.cc opt_choice_visitor.cc \
           arg.cc options.cc gapc.cc cpp.cc filter.cc \
           init_decls.cc cc.cc printer.cc index_visitor.cc \
           const.cc expr.cc statement.cc var_acc.cc \
           inline_nts.cc list_size_terminate.cc list_warn.cc list_visitor.cc \
           visitor.cc instance.cc product.cc fn_def.cc algebra.cc fn_decl.cc \
	   signature.cc type.cc \
	   runtime.cc alt.cc symbol.cc \
	   fn_arg.cc grammar.cc \
	   terminal.cc ast.cc log.cc \
	   driver.cc \
	   $(UNITTEST_CXX) \
           $(MODTESTS_CXX) \
	   testdata/test/range.cc \
	   testdata/test/code.cc testdata/test/indices.cc \
	   testdata/test/lexer.cc testdata/test/parser.cc testdata/test/tab.cc testdata/test/typecheck.cc \
	   testdata/test/product.cc testdata/test/listsizes.cc \
	   rtlib/string.cc \
	   $(GEN_CXXFILES) \
	   ambiguity_cfg_gen/generate_ambiguity_cfg.cc \
	   ambiguity_cfg_gen/var_info_item.cc \
	   ambiguity_cfg_gen/variable_context.cc \
	   ambiguity_cfg_gen/grammar_vm_function_compiler.cc \
	   ambiguity_cfg_gen/grammar_vm_command.cc \
	   ambiguity_cfg_gen/grammar_vm_stack.cc \
	   ambiguity_cfg_gen/grammar_vm.cc \
	   ambiguity_cfg_gen/grammar_vm_code.cc \
	   ambiguity_cfg_gen/parameter_position_attribute.cc \
	   ambiguity_cfg_gen/algebra_function_info_attribute.cc \
	   ambiguity_cfg_gen/transform_gap_to_cfg.cc \
	   ambiguity_cfg_gen/regular_expression_info_attribute.cc \
	   cfg/cfg.cc \
	   cfg/remove_unused_productions.cc \
	   util/symbol_table.cc \
	   ambiguity_cfg_gen/grammar_vm_function_exec_environment.cc \
	   util/attributable.cc \
	   util/attribute.cc \
	   util/annotate_the_set_first.cc \
	   util/annotate_cycles.cc \
	   util/annotate_algebra_function_names.cc \
	   util/annotate_reducible_attributes.cc \
	   util/annotate_dead_ends.cc \
	   util/naming_path.cc \
	   util/naming_domain.cc \
	   util/cycle_set_utils.cc \
	   util/remove_all_attributes.cc \
	   util/remove_cycle_set_attributes.cc \
	   util/remove_first_set_attributes.cc \
	   util/last_element_of_cycle_attribute.cc \
	   util/cycle_mark_attribute.cc \
	   util/cycle_set.cc \
	   util/cycle_attribute.cc \
	   util/algebra_function_name_attribute.cc \
	   util/grammar_production_naming_attribute.cc \
	   printer/gap.cc \
	   printer/cfg_pretty_print.cc \
	   printer/cfg_pretty_print_cout.cc \
	   specialize_grammar/actual_parameter_position_attribute.cc \
	   specialize_grammar/create_specialized_grammar.cc \
	   specialize_grammar/remove_cfg_cycles.cc \
	   specialize_grammar/set_of_cycle_sets.cc \
	   specialize_grammar/call_trace.cc \
	   specialize_grammar/rewrite_non_productive_cfg_rules.cc \
	   specialize_grammar/hidden_cfg_fragments_attribute.cc \
	   specialize_grammar/cycle_break_point_attribute.cc \
	   specialize_grammar/add_special_axiom_to_cfg.cc \
	   specialize_grammar/choice_function_application_attribute.cc \
	   specialize_grammar/designated_axiom_attribute.cc \
	   specialize_grammar/cycle_path_info_attribute.cc
DEPS = $(CXXFILES:.cc=.d)
OFILES = $(CXXFILES:.cc=.o)

# since bison 2.5 location/position.hh are not generated when
# location_type is defined
location.hh position.hh: testdata/config-tests/config.finished

location.hh: testdata/config-tests/location.hh
	cp $< $@

position.hh: testdata/config-tests/position.hh
	cp $< $@

# don't rely in which order make works on file lists
lexer.cc: parser.cc

parser.cc: testdata/config-tests/config.finished

# parser.o must be built first, since bison generates needed header files
parser.hh stack.hh: parser.cc

driver.o: parser.hh

$(filter-out parser.o lexer.o,$(OFILES)): location.hh position.hh stack.hh


include makefiles/deps.mf

ifdef NO_DEPS

# Brute force for compilers which do not support dependency
# generation, e.g. Sun CC
# for others the above location.hh ...: parser.y line is enough
$(filter-out parser.o lexer.o,$(OFILES)): parser.o

endif

EXEC_OBJ = $(filter testdata/modtest/%.o testdata/test/%.o ,$(OFILES)) gapc.o rtlib/string.o \
	   $(UNITTEST_CXX:.cc=.o)
MAIN_OBJ = $(filter-out $(EXEC_OBJ),$(OFILES))

LIBRNA_SRC = librna/rnalib.c librna/vienna/energy_par.c librna/vienna/fold_vars.c librna/vienna/read_epars.c librna/vienna/read_epars.c librna/vienna/params.c librna/vienna/utils.c
LIBRNA_OBJ = $(LIBRNA_SRC:.c=.o)
LIBRNA_PIO = $(LIBRNA_SRC:.c=.pio)

TEMP = $(GEN_CXXFILES) $(OFILES) $(DEPS)\
       $(TEST_TEMP) \
       parser.output parser.hh stack.hh location.hh \
       position.hh \
       gapc \
       $(MODTESTS) \
       stats testdata/modtest/stats.o \
       version.txt version.cc version.d version.o \
       testdata/config-tests/config.finished \
       prefix.cc prefix.d prefix.o \
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
  LDLIBS = -lboost_program_options$(BOOST_LIB_SUFFIX)

%: $(MAIN_OBJ) testdata/modtest/%.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)

gapc: LDLIBS =  -lboost_program_options$(BOOST_LIB_SUFFIX)

gapc: gapc.o $(MAIN_OBJ) version.o prefix.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)

	
# multi_rt_approx_rescore \

stats \
test_rt_tab \
multi_cyk multi_deps multi_indices multi_list_size multi_eliminate_lists_more multi_eliminate_lists multi_algebra multi_signature multi_rt_approx multi_rt_all multi_in_out multi_self_rec multi_calls multi_table_dim multi_max_filter multi_loops multi_ys parser: version.o prefix.o

backtrack: version.o

product typecheck tab indices listsizes dep: prefix.o version.o

range: LDLIBS =

range: testdata/modtest/range.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)


include makefiles/lexyaccxx.mf


.PHONY: clean
clean:
	rm -f $(TEMP)
	$(MAKE) -f config.mf clean

# test target: only triggers dependency tracking
.PHONY: depend
depend:
	touch dep


###############################################################################
# Shared libraries
###############################################################################

%.pio: %.c
	$(CC) -c -o $@ $(PIC_FLAGS) $(CPPFLAGS) $(CFLAGS) $<

%.fpio: %.c
	$(CC) -c -o $@ $(PIC_FLAGS) $(CPPFLAGS) $(CFLAGS_FP_FAST) $<


librna/rnalib.pio: CFLAGS = $(CFLAGS_OPT)

librna/librna$(SO_SUFFIX): LDLIBS = -lm

librna/librna$(SO_SUFFIX): $(LIBRNA_PIO)
	$(LD) $(SHARED_FLAGS) $^ $(LDLIBS) -o $@

librna/librnafast$(SO_SUFFIX): LDLIBS = -lm

librna/librnafast$(SO_SUFFIX): librna/rnalib.fpio $(filter-out librna/rnalib.pio, $(LIBRNA_PIO))
	$(LD) $(SHARED_FLAGS) $^ $(LDLIBS) -o $@


librna/librna.a: $(LIBRNA_OBJ)
	$(AR) -r $@ $^


################################################################################
# Mercurial version generation
################################################################################

ifeq "$(shell ls -d .hg 2>/dev/null)" ".hg"

version.txt: .hg/dirstate
	$(HG) log -r . --template '{date|isodate} {node} {tags}' > $@
	$(HG) id | grep '+' | $(SED) 's/^.\++.*$\/ wd modified/' >> $@

else

version.txt:
	basename `pwd` > version.txt

endif

version.cc: version.txt
	printf "#include \"version.hh\"\n\nnamespace gapc {\nconst char version_id[] = \"`cat $<`\";\n}\n" > $@

################################################################################
# PREFIX
################################################################################

prefix.cc: config.mf
	printf "#include \"prefix.hh\"\n\nnamespace gapc {\nconst char prefix[] = \"$(PREFIX)\";\nconst char systemsuffix[] = \"$(SYSTEM_SUFFIX)\";\n}\n" > $@


################################################################################
# Install
################################################################################

.PHONY: install

install: gapc librna/librna$(SO_SUFFIX)
	$(KSH) makefiles/run.sh $(PREFIX)

install-librna: librna/librna$(SO_SUFFIX)
	$(KSH) makefiles/run-librna.sh $(PREFIX)
	
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

$(UNIT_EXECS): LDLIBS=-lboost_unit_test_framework$(BOOST_LIB_SUFFIX)

testdata/unittest/runtime: runtime.o

testdata/unittest/rtlib: rtlib/string.o

testdata/unittest/rna.o: CPPFLAGS_EXTRA=-Ilibrna

testdata/unittest/rna: librna/librna.a

testdata/unittest/rna: LDLIBS=-lboost_unit_test_framework$(BOOST_LIB_SUFFIX) librna/librna.a

testdata/unittest/rna.d: CPPFLAGS_EXTRA=-Ilibrna

testdata/unittest/sequence.o unittest/sequence.d: CPPFLAGS_EXTRA=-Ilibrna -Irtlib

testdata/unittest/sample: LDLIBS=-lboost_unit_test_framework$(BOOST_LIB_SUFFIX) \
                        $(LIBGSL)

testdata/unittest/bench: LDLIBS=-lboost_unit_test_framework$(BOOST_LIB_SUFFIX) $(POSIXLIBS)

testdata/unittest/sample testdata/unittest/sample.o testdata/unittest/sample.d: CPPFLAGS_EXTRA=-DUSE_GSL


testdata/unittest/tablegen: testdata/unittest/expr_parser.o testdata/unittest/expr_lexer.o $(filter-out lexer.o parser.o driver.o, $(MAIN_OBJ)) version.o prefix.o

testdata/unittest/tablegen.o: testdata/unittest/expr_parser.o

testdata/unittest/expr_lexer.o: testdata/unittest/expr_parser.o

testdata/unittest/tracks testdata/unittest/productive: $(MAIN_OBJ) prefix.o version.o

testdata/unittest/%: testdata/unittest/%.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)


# modtest

MODTESTS_CXX:= $(wildcard testdata/modtest/*.cc)

MODTESTS:=$(sort $(subst testdata/modtest/,,$(MODTESTS_CXX:.cc=)))
modtests: $(MODTESTS)


# paraltest

paraltests: gapc librna/librna.a testdata/fp_eq

paraltest/fp_eq: LDFLAGS=$(C_LDFLAGS)

paraltest/fp_eq: LDLIBS=-lm

test: test-unit test-mod test-paral test-regress test-ambiguity

test-unit: unittests
	cd testdata/unittest &&\
	  $(KSH) run.sh $(subst testdata/unittest/,./,$(UNIT_EXECS))

test-mod: modtests
	cd testdata/modtest &&\
	$(KSH) run.sh ../../../$(TRUTHDIR)/Mod$(SYSTEM_SUFFIX) $(MODTESTS)

gen-mod: modtests
	cd testdata/modtest &&\
	  $(KSH) gen.sh ../../../$(TRUTHDIR)/Mod$(SYSTEM_SUFFIX) $(MODTESTS)

test-paral: paraltests
	cd testdata/paraltest &&\
	$(KSH) run.sh

test-regress: gapc librna/librna.a testdata/fp_eq testdata/regresstest/rand testdata/test/shapemfepfx/nowindow testdata/test/shapemfepfx/main testdata/test/shrepmfesample/main
	cd testdata/regresstest &&\
	$(KSH) run.sh ../../../$(TRUTHDIR)/Regress "" "$(KSH)" "$(LIBGSL)"

test-ambiguity: gapc
	cd testdata/ambiguitytest &&\
	$(KSH) run.sh ../../../$(TRUTHDIR)/Ambiguity "" "$(KSH)" "$(LIBGSL)"

testdata/test/shapemfepfx/nowindow testdata/test/shapemfepfx/main:
	$(MAKE) -C testdata/test/shapemfepfx all

testdata/test/shrepmfesample/main:
	$(MAKE) -C testdata/test/shrepmfesample all


