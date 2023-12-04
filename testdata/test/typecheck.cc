#include <iostream>
#include <string>
#include <cassert>

#include "../driver.hh"

#include "../log.hh"

#include "../signature.hh"


int main(int argc, char **argv)
{
  if (argc < 2) {
    std::cerr << "Supply filename!" << std::endl;
    return 1;
  }

  try {

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
    std::cerr << "Asm runtime by width: "
      << grammar->runtime_by_width() << std::endl;

    std::cerr << std::endl;

    std::cerr << "Exakt runtime (full table): "
      << grammar->runtime() << std::endl;


    std::cerr << *driver.ast.signature << std::endl;
    // FIXME
    hashtable<std::string, Type::Base*>::iterator i =
      driver.ast.types.find("alphabet");
    assert(i != driver.ast.types.end());
    dynamic_cast<Type::Alphabet*>(i->second)->temp = new Type::Char();
    std::cerr << *driver.ast.signature << std::endl;

    std::cerr << "Checking signature:" << std::endl;
    r = driver.ast.check_signature();

    if (r) {
      std::cerr << "Try to eliminate lists:" << std::endl;
      driver.ast.grammar->eliminate_lists();
    }

    std::cerr << "Checking algebras:" << std::endl;
    r = driver.ast.check_algebras();

    return 0;
  }
  return 1;

  } catch (std::exception &e) {
    std::cerr << "Exception: " << e.what() << '\n';
    return 1;
  }
}
