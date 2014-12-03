

#ifndef STACKLEN_HH
#define STACKLEN_HH


#include <rtlib/table.hh>
#include <rtlib/rna.hh>

template <typename T>
struct TA {
  typedef Table::Quadratic<T, Table::CYK> array_t;
  array_t &t;
  TA(array_t &a) : t(a) {}

  T &operator()(unsigned i, unsigned j)
  { return t.get_tabulated(i, j); }
  const T &operator()(unsigned i, unsigned j) const
  { return t.get_tabulated(i, j); }
  void operator()(unsigned i, unsigned j, const T &x)
  { t.get_tabulated(i, j) = x; }
};

template <typename C, typename U>
inline
std::pair<int, unsigned> stacklen(const Basic_Sequence<C> &seq, U a, U b)
{
  typedef Table::Quadratic<std::pair<int, unsigned>, Table::CYK> table_t;
  static table_t table;
  static bool compute = true;
  TA<std::pair<int, unsigned> > array(table);
  if (compute) {
    table.init(seq, "stacklen");
    unsigned n = seq.size();
    for (unsigned j = 2; j<=n; ++j) {
      for (unsigned is = j-2+1; is > 0; --is) {
        unsigned i = is-1;
        if (j-i < 5) {
          array(i, j).first = 0;
          array(i, j).second = 0;
          continue;
        }
        if (basepairing(seq, i, j)) {
          Subsequence s;
          s.i = i;
          s.j = j;
          s.seq = &seq;
          int nrg = sr_energy(s, s);
          if (j-i>3) {
            if (array(i+1, j-1).first + nrg > 0) { //array(i+1, j-1).first) {
              array(i, j).first = 0;
              array(i, j).second = 1;
            } else {
              array(i, j).first = array(i+1, j-1).first + nrg;
              array(i, j).second = array(i+1, j-1).second + 1;
            }
          } else {
            array(i, j).first = nrg;
            array(i, j).second = 1;
          }
        } else {
          array(i, j).first = 0;
          array(i, j).second = 0;
        }
      }
      array(j, j).first = 0;
      array(j, j).second = 0;
      array(j-1, j).first = 0;
      array(j-1, j).second = 0;
    }
    array(0, 0).first = 0;
    array(0, 0).second = 0;
    array(0, 1).first = 0;
    array(0, 1).second = 0;

    compute = false;
/*
    for (unsigned i = 0; i <= seq.size(); ++i) {
      for (unsigned j = i; j <= seq.size(); ++j) {
        std::cout << array.get_tabulated(i, j) << ' ';
      }
      std::cout << '\n';
    }
    */
  }
  return array(a, b);
}

template <typename A, typename B>
inline
A &first(std::pair<A, B> &p)
{
  return p.first;
}

template <typename A, typename B>
inline
B &second(std::pair<A, B> &p)
{
  return p.second;
}

template <typename A, typename B>
inline
const A &first(const std::pair<A, B> &p)
{
  return p.first;
}

template <typename A, typename B>
inline
const B &second(const std::pair<A, B> &p)
{
  return p.second;
}



#endif
