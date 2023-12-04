
#include "../driver.hh"
#include "../log.hh"

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cassert>


#include "../visitor.hh"

class A_Visitor : public Visitor {
  public:
    size_t rules, nts;
    A_Visitor() : rules(0), nts(0) {}
    void visit(Symbol::NT &n)
    {
      rules += n.alts.size();
      ++nts;
    }
    void visit_begin(Alt::Block &a)
    {
      rules += a.alts.size();
    }
};

int main(int argc, char **argv)
{
  if (argc != 2) {
    std::cerr << "Call " << *argv << " GRAMMAR\n";
    return 1;
  }
  std::ostringstream o;
  Log log;
  log.set_debug(false);
  log.set_ostream(o);
  Driver driver;
  std::string filename(argv[1]);
  driver.setFilename(filename);
  driver.parse();
  //assert(!driver.is_failing());
  if (driver.is_failing())
    return 4;

  Grammar *grammar = driver.ast.grammar();

  //see grammar->check_semantic();
  //
  bool b, r = true;
  b = grammar->init_tabulated();
  r = r && b;
  b = grammar->init_axiom();
  //assert(b);
  r = r && b;
  if (r)
    b = grammar->init_nt_links();
  r = r && b;
  grammar->remove_unreachable();
  //assert(r);
  b = ! grammar->has_nonproductive_NTs();
  r = r && b;
  //assert(r);
  if (!r)
    return 2;
  b = grammar->init_tracks();
  r = r && b;
  //assert(r);
  if (!r)
    return 3;

  grammar->init_multi_yield_sizes();

  b = !grammar->multi_detect_loops();
  if (!b)
    return 4;

  A_Visitor a;
  grammar->traverse(a);
  std::cout << "#NTs & #rules \n" << a.nts << ' ' << a.rules << '\n';

  return 0;
}

