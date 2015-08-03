
#include "../../src/driver.hh"
#include "../../src/log.hh"

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cassert>



#include "../../src/instance.hh"

#include "../../src/cpp.hh"

int main(int argc, char **argv)
{
  if (argc < 2) {
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

  grammar->multi_propagate_max_filter();

  grammar->init_table_dims();

  grammar->init_calls();

  grammar->init_self_rec();

  grammar->init_in_out();

  grammar->init_indices();

  grammar->dep_analysis();

  grammar->approx_table_conf();

  std::stringstream d;

  driver.ast.set_cyk();
  Printer::Cpp cpp(driver.ast, d); //std::cerr);
  cpp.print_cyk_fn(driver.ast);

  char buffer[100];
  while (d.getline(buffer, 100)) {
    std::string b(buffer);
    if (b.find("#ifndef _OPENMP") != b.npos)
      break;
  }
  while (d.getline(buffer, 100)) {
    std::string b(buffer);
    if (b.find("#else") == b.npos)
      std::cerr << b << '\n';
    else
      break;
  }

  return 0;
}

