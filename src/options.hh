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


#ifndef SRC_OPTIONS_HH_
#define SRC_OPTIONS_HH_

#include <string>
#include <vector>
#include <iostream>
#include <ostream>
#include <fstream>

#include <cassert>


std::string basename(const std::string &f);
std::string classname(const std::string &f);
std::string remove_dir(const std::string &f);


struct Options {
  Options()
    :  inline_nts(false), out(NULL), h_stream_(NULL), m_stream_(NULL),
      approx_table_design(false), tab_everything(false),
      cyk(false), backtrack(false), sample(false), subopt(false),
      kbacktrack(false),
      no_coopt(false),
      no_coopt_class(false),
      classified(false),
      window_mode(false),
      kbest(false),
      ambiguityCheck(false),
      specializeGrammar(false),
      verbose_mode(false),
      logLevel(3),
      pareto(0), multiDimPareto(false), cutoff(65),
      float_acc(0),
      specialization(0), step_option(0),
      plot_grammar(0), plotgrammar_stream_(NULL),
      derivative(0),
      gen_pytorch_interface(false) {
  }


  ~Options() {
    delete out;
    out = NULL;
    delete h_stream_;
    h_stream_ = NULL;
    delete m_stream_;
    m_stream_ = NULL;
    delete plotgrammar_stream_;
    plotgrammar_stream_ = NULL;
  }


  bool inline_nts;
  std::string in_file;
  std::string out_file;
  std::ostream *out;
  std::string header_file;
  std::ostream *h_stream_;
  std::string make_file;
  std::ostream *m_stream_;

  std::string instance;
  std::string product;
  std::string class_name;

  bool approx_table_design;
  bool tab_everything;
  bool cyk;
  bool backtrack;
  bool sample;
  bool subopt;
  bool kbacktrack;
  std::vector<std::string> tab_list;
  bool no_coopt;
  bool no_coopt_class;
  bool classified;

  bool window_mode;

  std::vector<std::string> includes;

  bool kbest;

  // Flag that signals if an ambiguity-cfg should be generated.
  // The name of the instance that is used to generate the string
  // grammar from the gapc-grammar is set as always: select nothing
  // for the first instance defined in the gapc-source-code, or
  // use the '-i'-option to explicitly name an instance name.
  bool ambiguityCheck;
  // Flag that signals whether a specializing GAP grammar is
  // requested by the user. This will prompt the compiler to
  // ignore most of the other options (except the instance selector
  // and output file name).
  bool specializeGrammar;
  // if not empty, automatically generate an outside version of
  // the provided grammar and print out results of the provided
  // list of outside non-terminals. Report ALL non-terminals,
  // if list states 'ALL'.
  std::vector<std::string> outside_nt_list;
  // flag that is used to turn on verbose mode, i.g. all suppressed
  // warnings and messages will be shown
  bool verbose_mode;
  // the log-level used for the Log-class.
  int logLevel;

  // switch for different pareto implementations
  int pareto;

  // multi dimensional pareto switch
  bool multiDimPareto;

  // cut-off value for yukish
  int cutoff;

  // number of digits used for pareto and sorting
  int float_acc;

  // switch for different ADP implementations
  int specialization;
  int step_option;

  bool is_stdout() {
    return out_file.empty();
  }


  bool has_instance() {
    return instance.empty();
  }


  std::ostream &stream() {
    if (is_stdout()) {
      return std::cout;
    }

    if (!out) {
      out = new std::ofstream(out_file.c_str());
      out->exceptions(std::ios_base::badbit |
                      std::ios_base::failbit | std::ios_base::eofbit);
    }
    return *out;
  }


  std::ostream &h_stream() {
    if (is_stdout()) {
      return std::cout;
    }

    if (!h_stream_) {
      h_stream_ = new std::ofstream(header_file.c_str());
      h_stream_->exceptions(std::ios_base::badbit |
                            std::ios_base::failbit | std::ios_base::eofbit);
    }
    return *h_stream_;
  }


  std::ostream &m_stream() {
    if (is_stdout()) {
      return std::cout;
    }

    assert(!m_stream_);
    m_stream_ = new std::ofstream(make_file.c_str());
    m_stream_->exceptions(std::ios_base::badbit |
                          std::ios_base::failbit | std::ios_base::eofbit);
    return *m_stream_;
  }

  std::string plot_grammar_file;
  int plot_grammar;
  std::ostream *plotgrammar_stream_;
  std::ostream &plotgrammar_stream() {
    if (is_stdout()) {
      return std::cout;
    }

    assert(!plotgrammar_stream_);
    plotgrammar_stream_ = new std::ofstream(plot_grammar_file.c_str());
    plotgrammar_stream_->exceptions(std::ios_base::badbit |
                          std::ios_base::failbit | std::ios_base::eofbit);
    return *plotgrammar_stream_;
  }

  /* a flag to compute first derivatives using inside/outside AKA forward/
   * backward AKA backpropagation schema. By default, set to 0, i.e. no
   * derivative computation. Set to 1, will generate code for computation
   * of first derivative. In the future, set to 2 shall generate additional
   * code for second derivative computation.
   */
  int derivative;

  bool gen_pytorch_interface;

  bool check();
};

#endif  // SRC_OPTIONS_HH_
