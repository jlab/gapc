
#include "../../src/driver.hh"
#include "../../src/log.hh"

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cassert>



#include "../../src/instance.hh"

#include "../../src/cpp.hh"
#include "../../src/cyk.hh"

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

  // gapc/front
  Driver driver;
  std::string filename(argv[1]);
  driver.setFilename(filename);
  driver.parse();
  //assert(!driver.is_failing());
  if (driver.is_failing())
    return 4;

  Grammar *grammar = driver.ast.grammar();
  bool r = grammar->check_semantic();
  if (!r) {
    return 5;
  }
  driver.ast.set_cyk();
  grammar->approx_table_conf();
  driver.ast.derive_temp_alphabet();
  r = driver.ast.check_signature();
  if (!r) {
    return 6;
  }
  r = driver.ast.check_algebras();
  if (!r) {
    return 7;
  }

  // gapc/back
  Instance *instance = driver.ast.first_instance;
  driver.ast.update_seq_type(*instance);
  r = driver.ast.insert_instance(instance);
  if (!r) {
    return 8;
  }
  driver.ast.instance_grammar_eliminate_lists(instance);
  grammar->init_list_sizes();
  grammar->init_indices();
  grammar->init_decls();
  grammar->dep_analysis();
  driver.ast.codegen();
  instance->codegen();

  std::stringstream d;
  Printer::Cpp cpp(driver.ast, d);
  Fn_Def *fn_cyk = print_CYK(driver.ast);
  cpp << *fn_cyk;

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

