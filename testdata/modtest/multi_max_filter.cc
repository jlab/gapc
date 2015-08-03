
#include "../../src/driver.hh"
#include "../../src/log.hh"

#include <iostream>
#include <sstream>
#include <cassert>

int main(int argc, char **argv)
{
  if (argc != 2) {
    std::cerr << "Call " << *argv << " GRAMMAR\n";
    return 1;
  }
  std::ostringstream o;
  Log log;
  log.set_debug(false);
  log.set_ostream(o);
  Driver driver;
  std::string filename(argv[1]);
  driver.setFilename(filename);
  driver.parse();
  //assert(!driver.is_failing());
  if (driver.is_failing())
    return 4;

  Grammar *grammar = driver.ast.grammar();

  //see grammar->check_semantic();
  //
  bool b, r = true;
  b = grammar->init_tabulated();
  r = r && b;
  b = grammar->init_axiom();
  //assert(b);
  r = r && b;
  if (r)
    b = grammar->init_nt_links();
  r = r && b;
  grammar->remove_unreachable();
  //assert(r);
  b = ! grammar->has_nonproductive_NTs();
  r = r && b;
  //assert(r);
  if (!r)
    return 2;
  b = grammar->init_tracks();
  r = r && b;
  //assert(r);
  if (!r)
    return 3;

  grammar->init_multi_yield_sizes();

  grammar->multi_detect_loops();

  grammar->multi_propagate_max_filter();

  const std::list<Symbol::NT*> &nts = grammar->nts();
  for (std::list<Symbol::NT*>::const_iterator i = nts.begin(); i != nts.end();
       ++i)
    std::cout << *(*i)->name << ' ' << (*i)->multi_ys() << '\n';

  return 0;
}

