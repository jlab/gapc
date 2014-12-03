#include "shapemfepfx.hh"
//include project_name.hh

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


#include "rtlib/string.hh"
#include "rtlib/list.hh"
#include "rtlib/hash.hh"
#include "rtlib/asymptotics.hh"

#include "rtlib/generic_opts.hh"

#include <iostream>

#include <cassert>

typedef shapemfepfx_insp_hash_h answerType;

int main(int argc, char **argv)
{
  gapc::Opts opts;
  try {
    opts.parse(argc, argv);
    //override window options
    opts.window_size = opts.inputs.front().second;
    opts.window_increment = 0;
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

#ifdef WINDOW_MODE
  gapc::return_type res = obj.run();
  unsigned int b;
  obj.t_0_left_most = 0;
  std::cout << "# window_i window_j subword_i subword_j shape prob mfe structure\n";
  for (b = 6; b <= opts.inputs.front().second; b++) {
	obj.t_0_right_most = b;

	typedef String pstring;
	intrusive_ptr<Backtrace<pstring, unsigned int> > bt = obj.backtrack(0, b);
	intrusive_ptr<Backtrace_List<pstring, unsigned int> > l = boost::dynamic_pointer_cast<Backtrace_List<pstring, unsigned int> >(bt);
	assert(l);
	double sum = 0;
	for (Backtrace_List<pstring, unsigned int>::iterator i = l->begin(); i != l->end(); ++i) {
		intrusive_ptr<Backtrace<pstring, unsigned int> > t = *i;
		assert(t);
		intrusive_ptr<Backtrace_Score<answerType::type, pstring, unsigned int> > u = boost::dynamic_pointer_cast<Backtrace_Score<answerType::type, pstring, unsigned int> >(t);
		assert(u);
		sum += u->score().second.second.pf.q1;
	}
	for (Backtrace_List<pstring, unsigned int>::iterator i = l->begin(); i != l->end(); ++i) {
		intrusive_ptr<Backtrace<pstring, unsigned int> > t = *i;
		assert(t);
		intrusive_ptr<Backtrace_Score<answerType::type, pstring, unsigned int> > u = boost::dynamic_pointer_cast<Backtrace_Score<answerType::type, pstring, unsigned int> >(t);
		assert(u);

		const answerType::type &score = u->score();
		double prob = u->score().second.second.pf.q1 / sum;

		intrusive_ptr<Eval_List<pstring> > eval = u->eval();
		for (Eval_List<pstring>::iterator i = eval->begin(); i != eval->end(); ++i) {
			std::cout << 0 << ';' << opts.inputs.front().second << ';' << 0 << ';' << b << ';' << score.first << ';' << prob << ';' << score.second.first.energy << ';' << *i << '\n';
		}
	}
  }
#else
  gapc::add_event("start");

  obj.cyk();
  gapc::return_type res = obj.run();

  gapc::add_event("end_computation");

  std::cout << "Answer: \n";
  obj.print_result(std::cout, res);

  gapc::add_event("end_result_pp");

#ifdef TRACE
  std::cerr << "start backtrack\n";
#endif
  for (unsigned int i = 0; i<opts.repeats; ++i)
    obj.print_backtrack(std::cout, res);
  obj.print_subopt(std::cout, opts.delta);

  gapc::add_event("end");
#endif

#ifdef STATS
  obj.print_stats(std::cerr);
#endif

  gapc::print_events(std::cerr);

  return 0;
}

