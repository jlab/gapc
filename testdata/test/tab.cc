#include "../driver.hh"

#include "../log.hh"

#include <iostream>
#include <string>

#include <boost/program_options.hpp>

namespace po = boost::program_options;

void parse_options(int argc, char **argv, po::variables_map &vm)
{
  po::options_description visible("Tabulation test helper options");
  visible.add_options()
    ("help,h", "produce help message")
    ("tab,t",  po::value< std::vector<std::string> >(), "set tab status of NT" )
    ("clear", "start from a clean table configuration")
    ("no-const", "don't take constant factors into account")
    ("thresh", po::value< unsigned int >(), "constant factor c; k+k/c is threshold, where k is the best constant facotr (default: c=5)")
    ("dot", "print graphiz .dot file")
    ("dot-tab", "print graphiz .dot file after tabulation")
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
  if (map.count("tab"))
    tabs = map["tab"].as< std::vector<std::string> >();

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
  if (map.count("dot")) {
    grammar->init_tabulated();
    grammar->init_axiom();
    grammar->init_nt_links();
    grammar->remove_unreachable();
    grammar->print_dot(std::cout);
    return 0;
  }
  bool r = grammar->check_semantic();
  if (!r) {
    std::cerr << "Exiting because of failed sematic check." << std::endl;
    return 1;
  }

  if (map.count("clear"))
    grammar->clear_tabulated();
  grammar->set_tabulated(tabs);

  grammar->init_in_out();

  grammar->print_nts();

  if (r) {
    std::cerr << "Asm optimal runtime by with: "
      << grammar->runtime_by_width() << std::endl;
    std::cerr << std::endl;
    grammar->put_table_conf(std::cerr);
    std::cerr << std::endl;
    std::cerr << "Asm optimal runtime under full table configuration: "
      << grammar->asm_opt_runtime() << std::endl;
    std::cerr << std::endl;
    grammar->put_table_conf(std::cerr);
    std::cerr << std::endl;
    std::cerr << "Exakt runtime: " << grammar->runtime() << std::endl;
    std::cerr << std::endl;
    grammar->put_table_conf(std::cerr);
    std::cerr << std::endl;
    driver.ast.warn_user_table_conf_suboptimal();
    std::cerr << std::endl;
    std::cerr << "Compute good table conf: " << std::endl;

    bool opt_const = true;
    if (map.count("no-const"))
      opt_const = false;
    unsigned int const_div = 5;
    if (map.count("thresh")) {
      const_div = map["thresh"].as< unsigned int >();
      if (!const_div) {
        std::cerr << "No div by zero allowed!" << std::endl;
        exit(1);
      }
    }
    grammar->approx_table_conf(opt_const, const_div);

    if (map.count("dot-tab"))
      grammar->print_dot(std::cout);
    std::cerr << std::endl;
    std::cerr << "Exakt runtime: " << grammar->runtime() << std::endl;
    grammar->put_table_conf(std::cerr);
    std::cerr << std::endl;
    grammar->print_nts();

    return 0;
  }
  return 1;
}
