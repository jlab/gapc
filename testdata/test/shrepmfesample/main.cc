#include "pfsampleshrep.hh"
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

typedef std::pair<std::pair<Shape, mfeanswer> , String> answerType;

int main(int argc, char **argv)
{
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

#ifdef WINDOW_MODE
  unsigned n = obj.t_0_seq.size();
  for (unsigned int i = 0; ; i+=opts.window_increment) {
    unsigned int right = std::min(n, i+opts.window_size);
    gapc::return_type res = obj.run();
    std::cout << "Answer ("
      << i << ", " << right << ") :\n";
    obj.print_result(std::cout, res);
    for (unsigned int j = 0; j<opts.repeats; ++j)
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

  std::map<Shape, std::pair<std::pair<mfeanswer, String>, unsigned int> > shapes;
  for (unsigned int k = 0; k < opts.repeats; k++) {
	  intrusive_ptr<Backtrace<answerType, unsigned int> >  bt = obj.backtrack();
	  intrusive_ptr<Backtrace_List<answerType, unsigned int> > l = boost::dynamic_pointer_cast<Backtrace_List<answerType, unsigned int> >(bt);
	  assert(l);
	  for (Backtrace_List<answerType, unsigned int>::iterator i = l->begin(); i != l->end(); ++i) {
	    intrusive_ptr<Backtrace<answerType, unsigned int> > t = *i;
	    assert(t);

	    intrusive_ptr<Eval_List<answerType> > eval = t->eval();
	    for (Eval_List<answerType>::iterator j = eval->begin(); j != eval->end(); ++j) {
	        std::map<Shape, std::pair<std::pair<mfeanswer, String>, unsigned int> >::iterator l = shapes.find((*j).first.first);
	        if (l == shapes.end()) {
	        	shapes[(*j).first.first] = std::make_pair(std::make_pair( (*j).first.second, (*j).second), 1);
	        } else {
	          if ((*j).first.second < l->second.first.first ) {
	            l->second.first.first = (*j).first.second;
	            l->second.first.second = (*j).second;
	          }
	          l->second.second++;
	        }
	    }
	  }
  }
  for (std::map<Shape, std::pair<std::pair<mfeanswer, String>, unsigned int> >::iterator i = shapes.begin(); i != shapes.end(); ++i) {
    std::cout << i->first << ' ' << double(i->second.second)/double(opts.repeats) << ' ' << i->second.first.first.energy << ' ' << i->second.first.second << '\n';
  }

//  std::cout << "Answer: \n";
//  obj.print_result(std::cout, res);

  gapc::add_event("end_result_pp");

#ifdef TRACE
  std::cerr << "start backtrack\n";
#endif
//  for (unsigned int i = 0; i<opts.repeats; ++i)
//    obj.print_backtrack(std::cout, res);
//  obj.print_subopt(std::cout, opts.delta);

  gapc::add_event("end");
#endif

#ifdef STATS
  obj.print_stats(std::cerr);
#endif

  gapc::print_events(std::cerr);

  return 0;
}

