#include "../driver.hh"

#include "../log.hh"

#include <iostream>
#include <string>

#include <boost/program_options.hpp>

namespace po = boost::program_options;

void parse_options(int argc, char **argv, po::variables_map &vm)
{
  po::options_description visible("Dependency analysis test helper options");
  visible.add_options()
    ("help,h", "produce help message")
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
  std::vector<std::string> tabs;

  po::variables_map map;
  parse_options(argc, argv, map);

  filename = map["file"].as<std::string>();

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
  if (!r) {
    std::cerr << "Exiting because of failed sematic check." << std::endl;
    return 1;
  }
  grammar->print_nts();

  grammar->dep_analysis();

  return 0;
}
