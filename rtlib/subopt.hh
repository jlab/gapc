/* {{{

    This file is part of gapc (GAPC - Grammars, Algebras, Products - Compiler;
      a system to compile algebraic dynamic programming programs)

    Copyright (C) 2008-2011  Georg Sauthoff
         email: gsauthof@techfak.uni-bielefeld.de or gsauthof@sdf.lonestar.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

}}} */

#ifndef SUBOPT_HH
#define SUBOPT_HH

#include <vector>

#include "table.hh"

template <typename pos_type = unsigned int>
class Marker {
  private:
    pos_type n;
    std::vector<bool> array;
    Table::DiagIndex<pos_type> index;
  public:
    Marker()
      : n(0)
    {
    }

    void init(pos_type x)
    {
      n = x;
      pos_type t = index(n);
      array.resize(t);
    }

    void set(pos_type i, pos_type j)
    {
      assert(i <= n);
      assert(j <= n);

      pos_type t = index(i, j, n);

      assert(t < array.size());

      array[t] = true;
    }

    bool is_set(pos_type i, pos_type j) const
    {
      assert(i <= n);
      assert(j <= n);

      pos_type t = index(i, j, n);

      assert(t < array.size());

      bool r = array[t];
      return r;
    }
};

template<typename pos_type>
inline
bool is_marked(Marker<pos_type> *marker, pos_type i, pos_type j)
{
  return marker->is_set(i, j);
}

template<typename pos_type>
inline
void mark(Marker<pos_type> &marker, pos_type i, pos_type j)
{
  marker.set(i, j);
}


// For subopt-shape-classify the bt-rhs is not needed in the first phase,
// 0 means empty, and isEmpty(pair) requires isEmpty(first) == isEmpty(second)
// thus, only for this mode we use one Backtrace_Dummy to fill the rhs != 0

template <typename Value, typename pos_int>
class Backtrace_Dummy : public Backtrace<Value, pos_int> {
  private:
  public:
    virtual intrusive_ptr<Eval_List<Value> > eval() { assert(0); return 0; };
};

template <typename Value, typename pos_int>
inline
intrusive_ptr<Backtrace<Value, pos_int> > dummy_bt(
    const intrusive_ptr<Backtrace<Value, pos_int> > &x)
{
  static intrusive_ptr<Backtrace<Value, pos_int> > t =
    new Backtrace_Dummy<Value, pos_int>();
  return t;
}


template<class T, typename pos_int, typename D, typename pos_type>
inline void push_back_min_subopt(List_Ref<T, pos_int> &x, T &e,
    D score,
    D delta,
    Marker<pos_type> &marker,
    pos_type i,
    pos_type j)
{
  assert(!isEmpty(e));

  if (left_most(e)-score > delta)
    return;

  // FIXME
  //x.ref().push_back(e);
  //return;

  // x.ref().push_back(e);
  // or keep only min/max for marking purposes:

  List<T, pos_int> &l = x.ref();
  if (l.isEmpty()) {
    l.push_back(e);
    return;
  }

  typename List<T, pos_int>::iterator itr = l.begin();
  typename List<T, pos_int>::iterator end = l.end();

  T a = *itr;

  ++itr;

  if (itr == end) {
    if (a < e) {
      l.push_back(e);
      return;
    }
    itr = l.begin();
    *itr = e;
    l.push_back(a);
    return;
  }

  if (e < a) {
    itr = l.begin();
    *itr = e;
    return;
  }

  T b = *itr;

  if (e > b) {
    *itr = e;
    return;
  }

}

template<class T, typename pos_int, typename D, typename pos_type>
inline void append_min_subopt(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e,
    D score,
    D delta,
    Marker<pos_type> &marker,
    pos_type u,
    pos_type v)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_min_subopt(x, *i, score, delta, marker, u, v);
}


// FIXME push_back_max_subopt ...
// FIXME add other subopt fns



#endif
