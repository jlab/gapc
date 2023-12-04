#include <iostream>
#include <string>
#include <cassert>

#include "../driver.hh"

#include "../log.hh"

#include "../signature.hh"

#include <boost/program_options.hpp>
namespace po = boost::program_options;

void parse_options(int argc, char **argv, po::variables_map &vm)
{
  po::options_description visible("Product tester options");
  visible.add_options()
    ("help,h", "produce help message")
    ("instance,i",  po::value< std::string >(), "use instance" )
    ;
  po::options_description hidden("");
  hidden.add_options()
    //("file", po::value<std::string>(&filename), "input file");
    ("file", po::value<std::string>(), "input file");
  po::positional_options_description pd; pd.add("file", 1);
  po::options_description opts;
  opts.add(hidden).add(visible);

  try {
    po::store(po::command_line_parser(argc, argv).options(opts)
        .positional(pd).run(), vm);
  } catch(std::exception &e) {
    std::cerr << e.what() << :: std::endl;
    exit(1);
  }
  po::notify(vm);

  if (vm.count("help")) {
    std::cout << visible << std::endl;
    exit(0);
  }

  if (!vm.count("file")) {
    std::cerr << "No filename specified." << std::endl;
    exit(1);
  }
}


int main(int argc, char **argv)
{
  std::string filename;
  std::string instance;

  po::variables_map map;
  parse_options(argc, argv, map);

  filename = map["file"].as<std::string>();
  if (map.count("instance"))
    instance = map["instance"].as< std::string >();

  std::cerr << filename << std::endl;
  Log log;
  log.set_debug();
  Driver driver;
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

    if (r) {
      std::cerr << "Checking products:" << std::endl;
      r = driver.ast.check_instances(0);
    }
    if (r) {
      driver.ast.print_instances(std::cerr);
    }
    std::cerr << r << std::endl;

    if (instance != "") {
      // driver.ast.grammar->reset_types(); is called by insert_instances()
      bool b = driver.ast.insert_instance(instance);
      if (b) {
        std::cerr << "Try to eliminate lists (algebra is applied):"
          << std::endl;
        driver.ast.instance_grammar_eliminate_lists(instance);
      }
    }


    return 0;
  }
  return 1;
}
