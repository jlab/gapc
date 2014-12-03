
#include <iostream>
#include <cstdlib>

#include <boost/program_options.hpp>
namespace po = boost::program_options;

int main(int argc, char **argv)
{
  po::variables_map vm;
  po::options_description visible("Options");
  visible.add_options()
    ("help,h", "produce help message")
    ("array", po::value< std::vector<std::string> >(), "array")
    ;
  po::options_description hidden("");
  hidden.add_options()
    ("file", po::value<std::string>(), "input file");
  po::positional_options_description pd; pd.add("file", 1);
  po::options_description opts;
  opts.add(hidden).add(visible);
  try {
    po::store(po::command_line_parser(argc, argv).options(opts)
        .positional(pd).run(), vm);
  } catch(std::exception &e) {
    std::cerr << e.what() << :: std::endl;
    std::exit(1);
  }
  po::notify(vm);
  if (vm.count("help"))
    return 0;
  return 1;
}
