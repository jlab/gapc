#include "../../src/driver.hh"
#include "../../src/log.hh"

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cassert>

int main(int argc, char **argv) {
  std::ostringstream o;
  Log log;
  log.set_debug(false);
  log.set_ostream(o);

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

  // inject rules for outside grammar
  //grammar->inject_outside_nts();

  // set approx table design
  if (grammar->tabulated.empty()) {
  	  grammar->approx_table_conf();
  }
	
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
	
  Instance i = driver.ast.first_instance;
  r = driver.ast.check_instances(i);
  if (!r)
    return 10;

  driver.ast.optimize_choice(&i);
  // apply this to identify standard functions like Min, Max, Exp etc.
  driver.ast.derive_roles();


  // ------------- back ------------
  grammar->init_list_sizes();

  grammar->init_indices();
  grammar->init_decls();
  // for cyk (ordering of NT for parsing, see page 101 of the thesis)
  grammar->dep_analysis();

  unsigned int nodeID = 1;
  grammar->to_dot(&nodeID, std::cout);
  
  return 0;
}
