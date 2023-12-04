#include <iostream>
#include <iomanip>
#include <sstream>
#include <cassert>

#include "../../src/driver.hh"
#include "../../src/log.hh"
#include "../../src/instance.hh"


int main(int argc, char **argv) {
  std::ostringstream o;
  Log log;
  log.set_debug(false);
  log.set_ostream(std::cout);

  Driver driver;

  if (argc != 2) {
    std::cerr << "Call " << *argv << " *.gap-file\n";
    return 1;
  }
  // === front
  // set the file name of the gap-source-code
  driver.setFilename(argv[1]);

  // parses the input file and builds the AST
  driver.parse();

  // simply gets the selected grammar, which is either the
  // grammar that occurred first in the source code or is the
  // one that was named in the parameters on the command line
  Grammar *grammar = driver.ast.grammar();

  // activate outside grammar generation
  std::vector<std::string> outside_nts;
  outside_nts.push_back("ALL");
  if (grammar->name->compare("canonicals_nonamb") == 0) {
    outside_nts.push_back("idonotexist");
  }
  driver.ast.set_outside_nt_list(&outside_nts);

  // Now check the semantic, which does more than the function
  // name suggests. The semantic check is embedded in the algorithm
  // that links the grammar graph together, and computes yield-sizes.
  // If the method returns false, there are some semantic errors,
  // which leads to the end of the compilation process.
  bool r = true;
  try {
    r = grammar->check_semantic();
  } catch (std::exception const &exc) {
    std::cerr << exc.what();
    return 0;
  }

  if (!r) {
    return 1;
  }

  grammar->approx_table_conf();

  // find what type of input is read
  // chars, sequence of ints etc.
  driver.ast.derive_temp_alphabet();

  r = driver.ast.check_signature();
  r = driver.ast.check_algebras();
  driver.ast.derive_roles();

  std::string instname = "";
  if (grammar->name->compare("pknotsRG") == 0) {
    instname = "mfeppenf";
  }
  Instance *instance = driver.ast.instance(instname);
  try {
    r = driver.ast.check_instances(instance);
  } catch (std::exception const &exc) {
    std::cerr << exc.what();
  }
}
