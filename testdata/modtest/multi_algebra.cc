
#include "../../src/driver.hh"
#include "../../src/log.hh"

#include <iostream>
#include <iomanip>
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

  b = !grammar->multi_detect_loops();
  if (!b)
    return 4;

  //grammar->multi_propagate_max_filter();

  // FIXME remove single track stuff later
  //if (grammar->axiom->tracks() > 1)
  //  return 6;
  /*
  if (grammar->axiom->tracks() == 1) {
  grammar->init_yield_sizes();
  b = ! grammar->detect_loops();
  if (!b)
    return 5;
  grammar->apply_max_filter();
  }

  grammar->init_table_dims();

  grammar->init_calls();

  grammar->init_self_rec();

  grammar->init_in_out();
  */

  driver.ast.derive_temp_alphabet();
  try {
    b = driver.ast.check_signature();
  } catch (LogThreshException) {
    return 7;
  }

  if (!b)
    return 8;

  log.set_debug(true);
  log.set_ostream(std::cerr);

  try {
    b = driver.ast.check_algebras();
  } catch (LogThreshException) {
    return 9;
  }

  std::cout << "Return: " << b << '\n';

  return 0;
}

