

#include <tabbed.hh>
#include <tools.hh>
#include <process.hh>

#include <tr1/unordered_map>
#include <cassert>
#include <sstream>
#include <cmath>
#include <sstream>
#include <fstream>
#include <iostream>

using namespace gs;
using namespace gs::tabbed;

struct value_t {
  double prob;
  int mfe;
  std::string structure;

  value_t() : prob(0), mfe(0) {}
  value_t(double p, int m, const std::string &s) : prob(p), mfe(m), structure(s) {}
};

typedef std::tr1::unordered_map<std::string, value_t> hash_t;

void add(hash_t &whash, const std::string &shape, double prob, int mfe,
    const std::string &structure)
{
  value_t value(prob, mfe, structure);
  assert(whash.find(shape) == whash.end());
  whash[shape] = value;
}

void compare(const std::string &seq, size_t i, size_t j, const hash_t &whash,
    const std::string &main)
{
  std::string subseq = seq.substr(i, j-i);

  hash_t hash;
  std::ostringstream o;
  process p;
  p.cmd(main).arg(subseq).out(o).run();
  std::istringstream o_in(o.str());
  for (iterator x = begin(o_in); x != end(); ++x) {
    std::string shape;
    double prob = 0;
    int mfe = 0;
    try {
      shape = x[0];
    } catch (gs::tabbed::exception::index) {
      continue;
    }
    prob = to_double(x[2]);
    mfe = to_int(x[1]);
    hash_t::const_iterator itr = whash.find(shape);
    if (itr == whash.end()) {
      std::cerr << "Subseq " << i << ' ' << j << ' ' << subseq << '\n';
      std::cerr << "Could not find shape: " << shape << '\n';
    }
    assert(itr != whash.end());
    value_t v = itr->second;
    if (std::fabs(prob - v.prob) >= 0.00001) {
      std::cerr << "Subseq " << i << ' ' << j << ' ' << subseq << '\n';
      std::cerr << "Shape: " << shape << '\n';
      std::cerr << "Probs: " << prob << ' ' << v.prob << '\n';
    }
    assert(std::fabs(prob - v.prob) < 0.00001);
    if (mfe != v.mfe) {
      std::cerr << "Subseq " << i << ' ' << j << ' ' << subseq << '\n';
      std::cerr << "Shape: " << shape << '\n';
      std::cerr << "Mfe: " << mfe << ' ' << v.mfe << '\n';
    }
    assert(mfe == v.mfe);
  }
}

int main(int argc, char **argv)
{
  assert(argc == 3);
  
  std::string windowmain(argv[1]);
  std::string main(argv[2]);
  std::string seq_name("../../input/rna100");

  std::string seq;
  std::ifstream f(seq_name.c_str()); //, std::ios_base::in);
  assert(f.good());
  f >> seq;
  assert(f.eof());

  //-p 0.08 -f ../../input/rna100 -w 20
  process p;
  std::ostringstream o;
  //p.cmd(windowmain).arg("-p").arg("0.08").arg("-f").arg(seq_name).
  p.cmd(windowmain).arg("-f").arg(seq_name).
    arg("-w").arg("20").out(o).run();
  std::istringstream wstream(o.str());
  size_t old_i = 0;
  size_t old_j = 0;
  hash_t whash;
  for (iterator i = begin(wstream, ';'); i != end(); ++i) {
    size_t window_i = 0;
    size_t window_j = 0;
    try {
      window_i = to_int(i[2]);
      window_j = to_int(i[3]);
    } catch (gs::tabbed::exception::index) {
      continue;
    }
    if ((window_i != old_i || window_j != old_j) && old_i != old_j){
      compare(seq, old_i, old_j, whash, main);
      hash_t t;
      swap(whash, t);
    }
    old_i = window_i;
    old_j = window_j;
    add(whash, i[4], to_double(i[5]), to_int(i[6]), i[7]);
  }
  compare(seq, old_i, old_j, whash, main);
  return 0;
}
