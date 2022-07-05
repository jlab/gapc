// Copyright 2022 stefan.m.janssen@gmail.com

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cassert>

#include "../../src/driver.hh"
#include "../../src/log.hh"

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Call " << *argv << " *.gap-file\n";
    return 1;
  }

  std::ostringstream o;
  Log log;
  log.set_debug(false);
  log.set_ostream(o);

  Driver driver;

  // === front
  // set the file name of the gap-source-code
  std::string filename(argv[1]);
  driver.setFilename(filename);

  // parses the input file and builds the AST
  driver.parse();
  if (driver.is_failing()) {
    return 4;
  }

  // simply gets the selected grammar, which is either the
  // grammar that occurred first in the source code or is the
  // one that was named in the parameters on the command line
  Grammar *grammar = driver.ast.grammar();
  // Now check the semantic, which does more than the function
  // name suggests. The semantic check is embedded in the algorithm
  // that links the grammar graph together, and computes yield-sizes.
  // If the method returns false, there are some semantic errors,
  // which leads to the end of the compilation process.
  bool r = grammar->check_semantic();
  if (!r) {
    return 2;
  }

  // set approx table design
  grammar->approx_table_conf();

  // find what type of input is read
  // chars, sequence of ints etc.
  driver.ast.derive_temp_alphabet();

  try {
    r = driver.ast.check_signature();
    if (!r) {
      return 3;
    }
  } catch (LogThreshException) {
    return 9;
  }

  if (driver.ast.first_instance == NULL) {
    return 11;
  }
  r = driver.ast.check_instances(driver.ast.first_instance);
  if (!r)
    return 10;

  // apply this to identify standard functions like Min, Max, Exp etc.
  driver.ast.derive_roles();


  // ------------- back ------------
  grammar->init_list_sizes();

  grammar->init_indices();
  grammar->init_decls();
  // for cyk (ordering of NT for parsing, see page 101 of the thesis)
  grammar->dep_analysis();

  unsigned int nodeID = 1;
  int plot_level = 99;
  grammar->to_dot(&nodeID, std::cout, plot_level);
  return 0;
}
