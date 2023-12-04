#include <iostream>
#include <string>
#include <cassert>

#include "../cc.hh"

#include "../driver.hh"

#include "../log.hh"

#include "../fn_def.hh"
#include "../instance.hh"


#include <boost/program_options.hpp>
namespace po = boost::program_options;

static void parse_options(int argc, char **argv, po::variables_map &vm)
{
  po::options_description visible("Code tester options");
  visible.add_options()
    ("help,h", "produce help message")
    ("inline,n", "try to inline NTs")
    ("instance,i",  po::value< std::string >(), "use instance" )
    ("algebras,a", "list algebras")
    ("print,p", "print instance code, see also -i")
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

void print_algebras(hashtable<std::string, Algebra*> &h)
{
  Printer::CC printer;
  for (hashtable<std::string, Algebra*>::iterator i =
      h.begin(); i != h.end(); ++i) {
    std::cerr << "Algebra " << i->first << ":" << std::endl;
    for (hashtable<std::string, Fn_Def*>::iterator j =
        i->second->fns.begin(); j != i->second->fns.end(); ++j) {
      std::cerr << "Function " << j->first << ":" << std::endl;
      printer.print(j->second->stmts);
      std::cerr << std::endl;
    }
    std::cerr << std::endl;
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
    // FIXME
    hashtable<std::string, Type::Base*>::iterator i =
      driver.ast.types.find("alphabet");
    assert(i != driver.ast.types.end());
    dynamic_cast<Type::Alphabet*>(i->second)->temp = new Type::Char();

    std::cerr << "Checking signature:" << std::endl;
    r = driver.ast.check_signature();

    std::cerr << "Checking algebras:" << std::endl;
    r = driver.ast.check_algebras();

    std::cerr << "Derive roles:" << std::endl;
    Log::instance()->set_debug();
    driver.ast.derive_roles();
    Log::instance()->set_debug(false);

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
        log.set_debug();
        std::cerr << "Try to eliminate lists (algebra is applied):"
          << std::endl;
        driver.ast.instance_grammar_eliminate_lists(instance);
        
        std::cerr << "Init list sizes:"
          << std::endl;
        driver.ast.grammar->init_list_sizes();
        driver.ast.warn_missing_choice_fns();

        if (map.count("inline"))
          driver.ast.grammar->inline_nts();

        driver.ast.grammar->print_nts();

        driver.ast.grammar->init_indices();
        driver.ast.grammar->print_indices();

        driver.ast.grammar->init_guards();
        driver.ast.grammar->print_guards();

        driver.ast.grammar->init_decls();
        std::cerr << std::endl << std::endl;

        driver.ast.codegen();
        Printer::CC cc;
        driver.ast.print_code(cc);

        if (map.count("algebras"))
          print_algebras(driver.ast.algebras);

        if (map.count("print")) {
          Instance *i = driver.ast.instance(instance);
          assert(i);
          i->codegen();
          std::cerr << std::endl << std::endl;
          i->print_code(cc);
        }

      }
    }
    return 0;
  }
  return 1;
}
