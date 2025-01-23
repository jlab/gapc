// include project_name.hh

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
#include <cassert>
#ifdef FLOAT_ACC
  #include <iomanip>
  #include <limits>
#endif
#include <algorithm>

#include "rtlib/string.hh"
#include "rtlib/list.hh"
#include "rtlib/hash.hh"
#include "rtlib/asymptotics.hh"
#include "rtlib/generic_opts.hh"

int main(int argc, char **argv) {
  gapc::Opts opts;
  try {
    opts.parse(argc, argv);
  } catch (std::exception &e) {
    std::cerr << "Exception: " << e.what() << '\n';
    std::exit(1);
  }
  gapc::class_name obj;

  try {
    obj.init(opts);
  } catch (std::exception &e) {
    std::cerr << "Exception: " << e.what() << '\n';
    std::exit(1);
  }

  // actual performance gains like 20%
  // see also http://www.ddj.com/cpp/184401305

  // workaround stupid Sun CC std::cout to fd0 after sync_with_stdio
  // with -m64 and stlport4 bug:
  // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6804239
#if defined(__SUNPRO_CC) && __SUNPRO_CC <= 0x5100
  #warning Enable sync_with_stdio because of Sun CC 12 Compiler Bug
#else
  std::ios_base::sync_with_stdio(false);
#endif
  std::cin.tie(0);

#ifdef FLOAT_ACC
  std::cout << std::setprecision(FLOAT_ACC) << std::fixed;
#endif

  // print statements prior to result list, e.g. for TikZ document generation
  obj.print_document_header(std::cout);
#ifdef WINDOW_MODE
  unsigned n = obj.t_0_seq.size();
  for (unsigned int i = 0; ; i+=opts.window_increment) {
    unsigned int right = std::min(n, i+opts.window_size);
    gapc::return_type res = obj.run();
    std::cout << "Answer ("
      << i << ", " << right << ") :\n";
    obj.print_result(std::cout, res);
    for (unsigned int j = 0; j < opts.repeats; ++j)
      obj.print_backtrack(std::cout, res);
    if (i+opts.window_size >= n)
      break;
    obj.window_increment();
  }
#else
  gapc::add_event("start");

  obj.cyk();
  gapc::return_type res = obj.run();

  gapc::add_event("end_computation");

#ifndef OUTSIDE
#ifndef TIKZ
  std::cout << "Answer: \n";
#endif
  obj.print_result(std::cout, res);
#else
  obj.report_insideoutside(std::cout);
#endif

  gapc::add_event("end_result_pp");

#ifdef TRACE
  std::cerr << "start backtrack\n";
#endif
  for (unsigned int i = 0; i < opts.repeats; ++i)
    obj.print_backtrack(std::cout, res);
  obj.print_subopt(std::cout, opts.delta);

  gapc::add_event("end");
#endif

  // print statements after result list, e.g. for TikZ document generation
  obj.print_document_footer(std::cout);
#ifdef STATS
  obj.print_stats(std::cerr);
#endif

  gapc::print_events(std::cerr);

  return 0;
}
