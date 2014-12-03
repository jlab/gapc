#include <iostream>
#include <string>

#include "../driver.hh"

#include "../log.hh"


int main(int argc, char **argv)
{
  if (argc < 2) {
    std::cerr << "Supply filename!" << std::endl;
    return 1;
  }
  Log log;
  log.set_debug();
  Driver driver;
  //driver.setStdin(true);
  std::string filename = std::string(argv[1]);
  driver.setFilename(filename);
  driver.parse();

  if (driver.is_failing()) {
    std::cerr << "Exiting because of previous errors." << std::endl;
    return 1;
  }

  Grammar *grammar = driver.ast.grammar;

  bool r = grammar->check_semantic();

  if (r) {
    std::cerr << "Asm runtime by with: "
      << grammar->runtime_by_width() << std::endl;

    std::cerr << std::endl;

    std::cerr << "Exakt runtime: " << grammar->runtime() << std::endl;

    return 0;
  }
  return 1;
}
