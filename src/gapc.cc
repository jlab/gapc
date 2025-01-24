/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2008-2011  Georg Sauthoff
         email: gsauthof@techfak.uni-bielefeld.de or gsauthof@sdf.lonestar.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

}}} */


#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <exception>
#include <memory>
#include <utility>
#include <boost/program_options.hpp>

#include "instance.hh"
#include "product.hh"

#include "cpp.hh"

#include "driver.hh"

#include "log.hh"

#include "version.hh"

#include "options.hh"
#include "backtrack.hh"
#include "subopt.hh"
#include "kbacktrack.hh"

#include "subopt_marker.hh"

#include "ambiguity_cfg_gen/generate_ambiguity_cfg.hh"
#include "printer/cfg_pretty_print.hh"
#include "printer/gap.hh"
#include "specialize_grammar/create_specialized_grammar.hh"
#include "outside/grammar_transformation.hh"
#include "outside/codegen.hh"

namespace po = boost::program_options;


static void version() {
  std::cout  << "gapc version " << gapc::version_id
        << std::endl << "  Copyright 2008-2011 Georg Sauthoff, GPL v3+"
        << std::endl << std::endl;
}


/*
 * Parse the command line options. This method is called from
 * the method Main::front() directly as the very first method
 * the compiler calls (although this is not an invariant one
 * should build on).
 */
static void parse_options(int argc, char **argv, Options *rec) {
  po::variables_map vm;
  po::options_description visible("Options");
  visible.add_options()
    ("help,h", "produce help message")
    ("inline,n", "try to inline NTs")
    ("instance,i",  po::value< std::string >(), "use instance (else first)" )
    ("product,p", po::value< std::string >(), "use product of algebras" )
    ("output,o", po::value< std::string >(), "output filename (out.cc)" )
    ("class-name", po::value< std::string >(), "default: basename(output)" )
    ("stdout,E", "print code to stdout")
    ("tab", po::value< std::vector<std::string> >(),
      "overwrite table conf with this list")
    ("table-design,t",
      "automatically compute optimal table configuration (ignore conf from "
      "source file)")
    ("tab-all", "tabulate everything")
    ("cyk", "bottom up evalulation codgen (default: top down unger style)")
    ("backtrace", "use backtracing for the pretty print RHS of the product")
    ("kbacktrace", "backtracing for k-scoring lhs")
    ("subopt-classify", "classified dp")
    ("subopt", "generate suboptimal backtracing code (needs foo * pretty)")
    ("sample", "generate stochastic backtracing code")
    ("no-coopt", "with kbacktrace, don't output cooptimal candidates")
    ("no-coopt-class", "with kbacktrace, don't output cooptimal candidates")
    ("window-mode,w", "window mode")
    ("kbest", "classify the k-best classes only")
    ("ambiguity",
      "converts the selected instance into a context free string grammar")
    ("specialize_grammar",
      "uses the selected instance and creates a GAP program which creates "
      "specialized GAP programs that recognize a subset of candidates of the "
      "original grammar.")
    ("outside_grammar", po::value< std::vector<std::string> >(),
      std::string("generate an outside version of the grammar and report "
      "outside results for an inside non-terminal. Provide multiple times for "
      "lists of non-terminals or type \"" + std::string(OUTSIDE_ALL) + "\" to "
      "report results for all non-terminals.\nRefer to --plot-grammar should "
      "you want 'see' the generated grammar.").c_str())
    ("verbose", "show suppressed warnings and messages")
    ("log-level,l", po::value<int>(),
      "the log level, valid values are 0 (VERBOSE), 1 (INFO),  2 (NORMAL), 3 "
      "(WARNING), 4 (ERROR). Default is 2 (NORMAL). INFO will e.g. report "
      "tabulated non-terminals and estimated run-time.")
    ("include,I", po::value< std::vector<std::string> >(), "include path")
    ("version,v", "version string")
    ("pareto-version,P", po::value<int>(),
      "Implementation of Pareto Product to use 0 (NoSort), 1 (Sort), 2 (ISort)"
      ", 3 (MultiDimOptimized), 4 (NoSort, domination ordered) ")
    ("multi-dim-pareto",
      "Use multi-dimensional Pareto. Works with -P 0, -P 1 and -P 3.")
    ("cut-off,c", po::value<int>(),
      "The cut-off value for -P 3 option (65 default).")
    ("float-accuracy,f", po::value<int>(),
      "The number of decimal places regarded for pareto and sorting procedures."
      " If this is not set the full floating point is compared.")
    ("specialized-adp,S", po::value<int>(),
      "Set to generate specialized implementations of the ADP framework: 0 "
      "(Standard), 1 (Sorted ADP), 2 (Pareto Eager ADP)")
    ("step-mode", po::value<int>(),
      "Mode of specialization: 0 force block mode, 1 force stepwise mode. This"
      " is automatically set to best option if not specified.")
    ("plot-grammar", po::value<int>(),
      "generates a Graphviz dot-file from the selected (and potentially "
      "modified) grammar.\nChoose a level (int) of detail:\n"
      "  0 (default) = no output at all\n"
      "  1 = grammar\n"
      "  2 = add indices\n"
      "  3 = add data types.\n"
      "  4 = add min/max yield sizes.\n"
      "  5 = add non-terminal table dimensions.\n"
      "(Use 'dot -Tpdf out.dot' to generate a PDF.)\nDefault file is out.dot")
    ("checkpoint", std::string(
     "enable periodic checkpointing of program progress.\n"
     "Useful for long running programs that might crash intermediately.\n"
     "You can continue from last checkpoint when re-executing the program.\n"
     "Checkpointing interval can be configured in the generated binary\n"
     "(creates new checkpoint every "
     + std::to_string(DEFAULT_CP_INTERVAL_MIN)
     + " minutes by default)\n").c_str());

  po::options_description hidden("");
  hidden.add_options()
    ("backtrack", "deprecated for --backtrace")
    ("kbacktrack", "deprecated for --kbacktrace")
    // ("file", po::value<std::string>(&filename), "input file");
    ("file", po::value<std::string>(), "input file");
  po::positional_options_description pd; pd.add("file", 1);
  po::options_description opts;
  opts.add(hidden).add(visible);

  po::store(po::command_line_parser(argc, argv).options(opts).positional(
    pd).run(), vm);

  po::notify(vm);

  if (vm.count("help")) {
    version();
    std::cout << "Usage: " << *argv << " (OPTION)* FILE\n\n";
    std::cout << visible << std::endl;
    std::exit(0);
  }

  if (vm.count("version")) {
    version();
    std::exit(0);
  }

  if (!vm.count("file")) {
    std::cerr << "No filename specified." << std::endl;
    std::exit(1);
  }
  rec->in_file = vm["file"].as<std::string>();
  if (vm.count("instance"))
    rec->instance = vm["instance"].as<std::string>();
  if (vm.count("product"))
    rec->product = vm["product"].as<std::string>();

  if (vm.count("output"))
    rec->out_file = vm["output"].as<std::string>();
  else
    //    rec.out_file = basename(rec.in_file) + ".cc";
    rec->out_file = "out.cc";
  rec->header_file = basename(rec->out_file) + ".hh";
  rec->make_file = basename(rec->out_file) + ".mf";

  if (vm.count("class-name"))
    rec->class_name = vm["class-name"].as<std::string>();
  else
    rec->class_name = classname(rec->out_file);

  if (vm.count("stdout"))
    rec->out_file.clear();

  if (vm.count("inline"))
    rec->inline_nts = true;

  if (vm.count("table-design"))
    rec->approx_table_design = true;
  if (vm.count("tab-all"))
    rec->tab_everything = true;
  if (vm.count("tab"))
    rec->tab_list = vm["tab"].as< std::vector<std::string> >();
  if (vm.count("include"))
    rec->includes = vm["include"].as< std::vector<std::string> >();
  if (vm.count("cyk"))
    rec->cyk = true;
  if (vm.count("backtrack") || vm.count("backtrace") || vm.count("sample"))
    rec->backtrack = true;
  if (vm.count("sample"))
    rec->sample = true;
  if (vm.count("subopt"))
    rec->subopt = true;
  if (vm.count("kbacktrack") || vm.count("kbacktrace"))
    rec->kbacktrack = true;
  if (vm.count("no-coopt"))
    rec->no_coopt = true;
  if (vm.count("no-coopt-class"))
    rec->no_coopt_class = true;
  if (vm.count("subopt-classify"))
    rec->classified = true;
  if (vm.count("window-mode"))
    rec->window_mode = true;
  if (vm.count("kbest"))
    rec->kbest = true;
  if (vm.count("ambiguity")) {
    rec->ambiguityCheck = true;
  }
  if (vm.count("specialize_grammar")) {
    rec->specializeGrammar = true;
  }
  if (vm.count("outside_grammar"))
    rec->outside_nt_list = vm["outside_grammar"]
      .as< std::vector<std::string> >();
  if (vm.count("verbose"))
    rec->verbose_mode = true;
  if (vm.count("log-level")) {
    int level = vm["log-level"].as<int>();
    rec->logLevel = level;
  }
  if (vm.count("pareto-version")) {
    int pareto = vm["pareto-version"].as<int>();
    rec->pareto = pareto;
  }

  if (vm.count("multi-dim-pareto")) {
    rec->multiDimPareto = true;
  }

  if (vm.count("cut-off")) {
    int cutoff = vm["cut-off"].as<int>();
    rec->cutoff = cutoff;
  }

  if (vm.count("float-accuracy")) {
    int float_acc = vm["float-accuracy"].as<int>();
    rec->float_acc = float_acc;
  }

  if (vm.count("specialized-adp")) {
    int spec = vm["specialized-adp"].as<int>();
    rec->specialization = spec;
  }

  if (vm.count("step-mode")) {
    int s = vm["step-mode"].as<int>();
    rec->step_option = s;
  }

  if (vm.count("plot-grammar")) {
    rec->plot_grammar = vm["plot-grammar"].as<int>();
    rec->plot_grammar_file = basename(rec->out_file) + ".dot";
  }

  if (vm.count("checkpoint")) {
    rec->checkpointing = true;
  }

  bool r = rec->check();
  if (!r) {
    throw LogError("Seen improper option usage.");
  }
}


/*
 * The main driver class for the gapc compiler. This is where
 * the whole processing starts from.
 */
class Main {
 private:
  int argc;
  char **argv;

  Log log;
  Options opts;
  // The driver is a global environment that is available to
  // all compiler methods and stages. It holds e.g. the AST,
  // This implies that some compoments in the driver are not
  // available at any time but only after they have been build
  // by some part or stage of the compiler.
  Driver driver;

  Code::Gen code_;

  // classified product are replaced by times product
  void conv_classified_product(Options *opts) {
    Instance *instance = driver.ast.instance(opts->instance);
    if (!instance) {
      return;
    }
    bool r = instance->replace_classified_product();
    if (r) {
      opts->classified = true;
    }
  }

 private:
  /*
   * The front end of the compiler. This method parses all
   * command line parameters, reads the program text of the
   * gap-file, creates a AST from this source code, and
   * configures the driver class.
   * If automated table design is activated by the command
   * line parameters, this is the place where the AST is
   * annotated with tabulation information, which makes the
   * AST look the same as with a user provided table design,
   * except for differences in choice of the set of non-terminals
   * marked for tabulation.
   */
  void front() {
    // parses the command line options and fills the Options
    // instance "opts".
    parse_options(argc, argv, &opts);

    // before we do anything, set the log-level. Create a new
    // instance, which automatically will
    assert(Log::instance());
    if (opts.verbose_mode) {
      Log::instance()->setLogLevel(Log::VERBOSE);
    } else {
      Log::instance()->setLogLevel(Log::LogLevel(opts.logLevel));
    }

    // set the file name of the gap-source-code
    driver.setFilename(opts.in_file);
    driver.set_includes(opts.includes);

    // parses the input file and builds the AST
    driver.parse();
    // Sets the active "instance" used for translation. For
    // more information on this see the comment of the method
    // AST::select_grammar (instance_name).
    driver.ast.select_grammar(opts.instance);
    driver.ast.set_outside_nt_list(&opts.outside_nt_list);
    driver.parse_product(opts.product);

    if (driver.is_failing()) {
      throw LogError("Seen parse errors.");
    }

    // simply gets the selected grammar, which is either the
    // grammar that occured first in the source code or is the
    // one that was named in the parameters on the command line
    Grammar *grammar = driver.ast.grammar();
    // Now check the semantic, which does more than the function
    // name suggests. The semantic check is embedded in the algorithm
    // that links the grammar graph together, and computes yield-sizes.
    // If the method returns false, there are some semantic errors,
    // which leads to the end of the compilation process.
    bool r = grammar->check_semantic();
    if (!r) {
      throw LogError("Seen semantic errors.");
    }

    // transform inside grammar into an outside one, if user requests
    if (driver.ast.outside_generation()) {
      grammar->convert_to_outside();
    }

    // configure the window and k-best mode
    driver.ast.set_window_mode(opts.window_mode);
    driver.ast.kbest = Bool(opts.kbest);

    if (opts.cyk) {
      driver.ast.set_cyk();
    }

    // Generate a table design, depending on the options set
    // by the user.
    if (opts.tab_everything) {
      grammar->set_all_tabulated();
    }
    if (!opts.tab_list.empty()) {
      grammar->clear_tabulated();
      grammar->set_tabulated(opts.tab_list);
    }
    if (opts.approx_table_design) {
      grammar->approx_table_conf();
    }
    // TODO(sjanssen): better write message to Log instance, instead of
    // std::cout directly!
    if (Log::instance()->is_verbose()) {
      std::cout << "The following non-terminals will be tabulated (%name "
        << "indicates linear table, %name% indicates constant tables):\n  ";
      grammar->put_table_conf(std::cout);
      std::cout << "\n\n";
      std::cout << "resulting in an estimated runtime of:\n  ";
      std::cout << grammar->runtime() << '\n';
    }

    // After the table configuration is generated, check for
    // suboptimal designs and present a message to the user.
    driver.ast.warn_user_table_conf_suboptimal();

    // find what type of input is read
    // chars, sequence of ints etc.
    driver.ast.derive_temp_alphabet();

    r = driver.ast.check_signature();
    if (!r) {
      throw LogError("Seen signature errors.");
    }
    r = driver.ast.check_algebras();
    if (!r) {
      throw LogError("Seen algebra errors.");
    }
    // apply this to identify standard functions like Min, Max, Exp etc.
    driver.ast.derive_roles();
  }


  /*
   * The back-end of the compiler. If the instance is a classified product,
   * this method gets called twice: once for the first algebra
   * of the product, then for both algebras of the product.
   */
  void back(Instance *i = 0, Instance *instance_buddy = 0) {
    Instance *instance = i;
    if (!i || instance_buddy) {
      if (opts.backtrack || opts.subopt || opts.kbacktrack) {
        instance = driver.ast.split_instance_for_backtrack(opts.instance);
      } else {
        instance = driver.ast.instance(opts.instance);
        if (instance) {
          bool r = driver.ast.check_instances(instance);
          if (!r) {
            throw LogError("Instance checks failed.");
          }
        }
        if (instance && instance->product->contains(Product::OVERLAY)) {
          LogError("Overlay product '|' only make sense with --backtrack");
        }
      }
      if (!instance) {
        throw LogError("Seen instance errors.");
      }
    }

    bool nullarySort = false;
    if (opts.pareto > 0) {
        driver.ast.set_pareto_version(*instance, opts.pareto);

        if (opts.pareto == 1 || opts.pareto == 3 || opts.pareto == 4) {
            nullarySort = true;
            if ((opts.backtrack || opts.subopt || opts.kbacktrack)) {
              if (opts.multiDimPareto) {
                driver.ast.set_back_track_paretosort(Product::MULTI);
              } else {
                driver.ast.set_back_track_paretosort(Product::STANDARD);
              }
            }

            if (opts.multiDimPareto) {
                instance->product->set_sorted_choice(Product::MULTI);
            } else {
                instance->product->set_sorted_choice(Product::STANDARD);
            }
        }
    }

    if (opts.specialization > 0) {
        if (nullarySort && opts.specialization != 1) {
            if (opts.specialization == 1) {
                instance->product->set_sorted_choice(Product::NULLARY_SORTER);
                if ((opts.backtrack || opts.subopt || opts.kbacktrack)) {
                    driver.ast.set_back_track_paretosort(
                      Product::NULLARY_SORTER);
                }
            } else if (opts.pareto == 0) {
                instance->product->set_sorted_choice(
                  Product::NULLARY_COMPERATOR);
                if ((opts.backtrack || opts.subopt || opts.kbacktrack)) {
                    driver.ast.set_back_track_paretosort(
                      Product::NULLARY_COMPERATOR);
                }
            } else {
                instance->product->set_sorted_choice(
                  Product::NULLARY_COMPERATOR_SORTER);
                if ((opts.backtrack || opts.subopt || opts.kbacktrack)) {
                    driver.ast.set_back_track_paretosort(
                      Product::NULLARY_COMPERATOR_SORTER);
                }
            }
        } else {
            if (opts.specialization == 1) {
                instance->product->set_sorted_choice(Product::SORTER);
                if ((opts.backtrack || opts.subopt || opts.kbacktrack)) {
                    driver.ast.set_back_track_paretosort(Product::SORTER);
                }
            } else if (opts.pareto == 0) {
                instance->product->set_sorted_choice(Product::COMPERATOR);
                if ((opts.backtrack || opts.subopt || opts.kbacktrack)) {
                    driver.ast.set_back_track_paretosort(
                      Product::COMPERATOR);
                }
            } else {
                instance->product->set_sorted_choice(
                  Product::COMPERATOR_SORTER);
                if ((opts.backtrack || opts.subopt || opts.kbacktrack)) {
                    driver.ast.set_back_track_paretosort(
                      Product::COMPERATOR_SORTER);
                }
            }
        }

        if (opts.backtrack || opts.subopt || opts.kbacktrack) {
             driver.ast.code_mode().set_keep_cooptimal(false);
        } else {
             driver.ast.code_mode().set_keep_cooptimal(!opts.no_coopt_class);
        }
    }


    driver.ast.set_float_accuracy(*instance, opts.float_acc);
    if (opts.pareto == 3) {
        driver.ast.set_pareto_cutoff(*instance, opts.cutoff);
    }

    if (opts.multiDimPareto) {
         if (opts.pareto == 0 || opts.pareto == 1 || opts.pareto == 3 ||
             opts.pareto == 4) {
             driver.ast.set_pareto_dim(*instance, true);
         } else {
            throw LogError(
              "Multi-Dimensional Pareto is only available for Pareto type 0, 1"
              " and 3.");
         }
    }

    driver.ast.set_adp_header(opts.specialization, opts.pareto,
                              opts.multiDimPareto, opts.step_option);

    // set the alphabet type in all VAR_DECL
    driver.ast.update_seq_type(*instance);

    if (opts.no_coopt) {
      driver.ast.instance_->product->set_no_coopt();
    }
    if (opts.no_coopt_class) {
      driver.ast.instance_->product->set_no_coopt_class();
    }

    bool r = driver.ast.insert_instance(instance);
    if (!r) {
      throw LogError("Instance inserting errors.");
    }

    // no many results for single backtrace
    // or overlay does not define backtrace
    driver.ast.check_backtrack(opts);

    // remove lists in the definitions where-ever possible
    // for example for Min or max choice functions
    driver.ast.instance_grammar_eliminate_lists(instance);

    if (opts.checkpointing) {
      if (driver.ast.checkpoint) {
        /*
           delete Checkpoint class if it exists already;
           this is only the case if a classified/interleaved
           product is parsed, in which case this method (back)
           gets called twice: once for the buddy out class (1st call)
           and once for the regular out class (2nd call)
        */
        delete driver.ast.checkpoint;
      }

      Printer::Checkpoint *cp = new Printer::Checkpoint();
      driver.ast.checkpoint = cp;

      if (opts.classified) {
        /*
           true if product is classified/interleaved product;
           for this type of product, a buddy class is generated,
           whose tables don't need to be checkpointed though,
           so the table types will not be checked for compatibility;
           this method will be called once more for the actual class,
           in which case opts.classified will be false,
           so the tables are regularly checked for compatibility and
           the checkpointing routine will be inserted if that's the case
        */
        cp->is_buddy = true;
      } else {
        bool answer_type_supported = cp->is_supported(driver.ast.grammar()->
                                                      tabulated);

        // only enable checkpointing if type of every table is supported
        if (answer_type_supported) {
          if (opts.cyk) {
            cp->cyk = true;
          }
          Log::instance()->normalMessage("Checkpointing routine integrated.");
        } else {
          Log::instance()->error("Checkpointing routine could not be "
                                 "integrated, because (one of) the table "
                                 "type(s) can't be serialized.");
          opts.checkpointing = false;
          delete cp;
          std::exit(0);
        }
      }
    }

    Grammar *grammar = driver.ast.grammar();
    grammar->init_list_sizes();
    driver.ast.warn_missing_choice_fns(instance);

    // inlining will remove sub NTs when possible by bringing
    // the subcontent to the original grammar role
    if (opts.inline_nts) {
      grammar->inline_nts();
    }

    grammar->init_indices();
    grammar->init_decls();
    // for cyk (ordering of NT for parsing, see page 101 of the thesis)
    grammar->dep_analysis();

    driver.ast.set_adp_version(*instance, opts.specialization,
                               opts.step_option, opts.pareto);

    driver.ast.codegen();

    instance->codegen();

    if (opts.specialization == 0) {
        driver.ast.optimize_choice(*instance);
        driver.ast.optimize_classify(*instance);
    } else {
        Log::instance()->warning(
          "Choice function and classification optimization are disabled for "
          "specialized ADP.");
    }

    // to ease inspection of the selected grammar, one can create a graphviz
    // dot-file for the grammar. This is handy if gapc modifies the original
    // grammar from the source file.
    // activate with command line argument --plot-grammar
    if (opts.plot_grammar > 0) {
      unsigned int nodeID = 1;
      grammar->to_dot(&nodeID, opts.plotgrammar_stream(), opts.plot_grammar);
      Log::instance()->normalMessage(
        "Graphviz representation of selected grammar has been saved in '"
        + opts.plot_grammar_file + "'.\nUse e.g. 'dot -Tpdf "
        + opts.plot_grammar_file + " > foo.pdf' to generate a PDF.");
    }

    driver.ast.set_class_name(opts.class_name);


    // Paste out the header file of the compile-result.
    // The Printer::Cpp writes out its components on
    // each call.
    // NOTE: the output stream for the header file is used
    // as a destination for the next lines. This is not the only
    // place where the stream is written. The method Main.finish()
    // also writes some lines to the header file.
    Printer::Cpp hh(driver.ast, opts.h_stream());
    hh.set_argv(argv, argc);
    hh.class_name = opts.class_name;
    hh.header(driver.ast);
    hh.begin_fwd_decls();
    driver.ast.print_code(hh);
    instance->print_code(hh);
    hh.footer(driver.ast);
    hh.end_fwd_decls();
    hh.header_footer(driver.ast);
    if (driver.ast.outside_generation()) {
      print_insideoutside_report_fn(hh, driver.ast);
    }

    // Write out the C++ implementation file of the
    // compile-result.
    Printer::Cpp cc(driver.ast, opts.stream());
    cc.set_argv(argv, argc);
    cc.class_name = opts.class_name;
    cc.set_files(opts.in_file, opts.out_file);
    cc.prelude(opts, driver.ast);
    cc.imports(driver.ast);
    cc.global_constants(driver.ast);
    driver.ast.print_code(cc);
    instance->print_code(cc);
    cc.footer(driver.ast);

    Code::Gen code(driver.ast);
    code_ = code;

    std::unique_ptr<Backtrack_Base> bt;
    if (opts.backtrack) {
      bt = std::unique_ptr<Backtrack_Base>(new Backtrack());
    } else if (opts.subopt) {
      bt = std::unique_ptr<Backtrack_Base>(new Subopt());
    } else if (opts.kbacktrack) {
      bt = std::unique_ptr<Backtrack_Base>(new KBacktrack());
    } else if (opts.classified) {
      bt = std::unique_ptr<Backtrack_Base>(new Subopt_Marker());
    }

    if (bt.get()) {
      if (opts.specialization > 0) {
           driver.ast.code_mode().set_keep_cooptimal(!opts.no_coopt_class);
      }

      driver.ast.backtrack_gen(*bt);
      bt->print_header(hh, driver.ast);
      bt->print_body(cc, driver.ast);
    }

    hh.backtrack_footer(driver.ast);

    hh.close_class();
  }


  /*
   * This is a kind of finalizer method for the compiler
   * that is called at the end after all processing has
   * taken place.
   */
  void finish() {
    Printer::Cpp hh(driver.ast, opts.h_stream());
    hh.class_name = opts.class_name;
    hh.typedefs(code_);
  }


  /*
   * Writes out the make file which makes it easier for the end user
   * to compile the result of this gapc compiler.
   * Precondition: the AST must have been created and configured.
   */
  void makefile() {
    Printer::Cpp mm(driver.ast, opts.m_stream());
    mm.set_argv(argv, argc);
    mm.makefile(opts);
  }

 public:
  /*
   * Inits a new instance of the compiler. Actually there
   * is nothing to initialize, other than the fields that
   * hold the passed argument values.
   */
  Main(int a, char **v) : argc(a), argv(v) {
  }


  /*
   * This is the entry point where the software starts.
   */
  void runKernal() {
    makefile();

    conv_classified_product(&opts);

    if (opts.classified) {
      std::string class_name = opts.class_name;
      bool kbacktrack = opts.kbacktrack;
      opts.kbacktrack = false;
      opts.class_name += "_buddy";

      std::pair<Instance*, Instance*> r =
        driver.ast.split_classified(opts.instance);
      back(r.first);

      // FIXME init marker code in buddy ...

      opts.cyk = false;
      opts.class_name = class_name;
      opts.classified = false;
      opts.kbacktrack = kbacktrack;
      driver.ast.check_instances(r.second);
      Code::Mode mode;
      mode.set_subopt_buddy();
      driver.ast.set_code_mode(mode);

      back(r.second, r.first);
    } else {
      back();
    }

    finish();
  }


  void runAmbuigutyCFGGenerator() {
    // The ambiguity check is base on the selected instance,
    // which can be obtained from the AST.
    Instance* instance = driver.ast.instance(opts.instance);

    // check if the named instance is present in the gap-program
    if (instance == NULL) {
      throw LogError(
        "No instance with name '" + opts.instance +
        "' defined in the source code.");
    }

    // get the selected product of the instance and extract
    // the algebra from it. The product itself has a accessor
    // method which returns the algebra.
    Product::Base *product = instance->product;

    // the algebra must be a simple single algebra, not any
    // kind of product
    if (!product->is(Product::SINGLE)) {
      throw LogError(instance->loc(),
        "For ambiguity checking it is required to use an instance that "
        "uses not a product of algebras, but simply a single algebra.");
    }

    Algebra* canonical_algebra = product->algebra();
    Grammar* grammar = instance->grammar();

    // Create a file name for the cfg file...
    std::string cfgFileName = basename(opts.out_file) + ".cfg";
    // ...and the appropriate std::ostream for this.
    std::ofstream oStream(cfgFileName.c_str());

    // Check if the axiom is set. This is more an internal error than
    // a programmar's fault.
    if (grammar->axiom == NULL) {
      throw LogError("gap-00160: No axiom given!");
    }

    // One last parameter, before we start the ambiguity CFG generator
    // is the command line call that led us to the output to come:
    std::string commandLineCall = assembleCommandLineCall();


    // Create a cfg generator, and start with processing the
    // AST and generating a CFG output file.
    AmbiguityCFG::GenerateAmbiguityCFG generator;
    CFG::CFG* cfg = generator.generateCFG(canonical_algebra, grammar->axiom);

    // Print out the result CFG into the output stream.
    Printer::CFGPrettyPrint ppCFG(oStream);
    ppCFG.setCommandLineCall(new std::string(commandLineCall));
    ppCFG.prettyPrint(cfg);
  }


  void runSpecializingGrammarGenerator() {
    SpecializeGrammar::CreateSpecializedGrammar grammarSpecializer;
    AST* newAst = grammarSpecializer.createGrammar(
      &driver.ast, &opts.instance);

    // Create a file name for the cfg file...
    std::string cfgFileName = basename(opts.out_file) + ".gap";
    // ...and the appropriate std::ostream for this.
    std::ofstream oStream(cfgFileName.c_str());
    // Printer::GapPrinter gapp (std::cout);
    Printer::GapPrinter gapp(oStream);
    gapp.print(newAst);
  }


  std::string assembleCommandLineCall() {
    std::ostringstream cmdLineCall;
    bool firstLoopRun = true;
    for (int i = 0; i < argc; ++i) {
      if (!firstLoopRun) {
        cmdLineCall << ' ';
      }
      cmdLineCall << argv[i];
      firstLoopRun = false;
    }
    std::string commandLineCall(cmdLineCall.str());
    return commandLineCall;
  }


  void run() {
    // We need the front end because that is the place where
    // some post-processing of the AST takes place. Also this
    // is place where the command-line options are parsed.
    front();

    // If we simply want to check the ambiguity of an instance,
    // we do nothing else.
    if (opts.ambiguityCheck) {
      runAmbuigutyCFGGenerator();
    } else if (opts.specializeGrammar) {
      runSpecializingGrammarGenerator();
    } else {
      runKernal();
    }
  }
};


/*
 * The official starting point for the program. This is simply
 * a stub for the compiler to call the actual main method of
 * gapc.
 */
int main(int argc, char **argv) {
  Main m(argc, argv);
  try {
    m.run();
  }
  catch (std::exception &e) {
    std::cerr << e.what() << '\n';
    return 1;
  }
  return 0;
}
