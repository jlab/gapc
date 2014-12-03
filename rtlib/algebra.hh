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


#ifndef RT_ALGEBRA_HH
#define RT_ALGEBRA_HH

#include "empty.hh"

#include <algorithm>
#include <numeric>

// FIXME remove
//#include <iostream>



template <typename Itr>
inline
typename std::iterator_traits<Itr>::value_type
minimum(Itr begin, Itr end)
{
  if (begin == end) {
    typename std::iterator_traits<Itr>::value_type r;
    empty(r);
    return r;
  }
#ifndef NDEBUG
  for (Itr i = begin; i != end; ++i)
    assert(!isEmpty(*i));
#endif
  Itr r = std::min_element(begin, end);
  return *r;
}

template <typename Iterator>
inline
typename std::iterator_traits<Iterator>::value_type
minimum(std::pair<Iterator, Iterator> &p)
{
  return minimum(p.first, p.second);
}

template <typename T>
inline
T minimum(T t)
{
  return t;
}

template <typename T>
inline
T maximum(T t)
{
  return t;
}

template <typename T>
inline
T sum(T t)
{
  return t;
}

template <typename Itr>
inline
typename std::iterator_traits<Itr>::value_type
maximum(Itr begin, Itr end)
{
  if (begin == end) {
    typename std::iterator_traits<Itr>::value_type r;
    empty(r);
    return r;
  }
#ifndef NDEBUG
  for (Itr i = begin; i != end; ++i)
    assert(!isEmpty(*i));
#endif
  Itr r = std::max_element(begin, end);
  return *r;
}

template <typename Iterator>
inline
typename std::iterator_traits<Iterator>::value_type
maximum(std::pair<Iterator, Iterator> &p)
{
  return maximum(p.first, p.second);
}

template <typename Itr>
inline
typename std::iterator_traits<Itr>::value_type sum(Itr begin, Itr end)
{
  typename std::iterator_traits<Itr>::value_type n;
  if (begin == end) {
    empty(n);
    return n;
  }
  assert(!isEmpty(*begin));
  n = *begin;
  ++begin;
  for (; begin != end; ++begin) {
    assert(!isEmpty(*begin));
    n += *begin;
  }
  return n;
}

template <typename Iterator>
inline
typename std::iterator_traits<Iterator>::value_type
sum(std::pair<Iterator, Iterator> &p)
{
  return sum(p.first, p.second);
}

#include <math.h>

template <typename T>
inline
T expsum(T t)
{
  return t;
}

template <typename Itr>
inline
typename std::iterator_traits<Itr>::value_type expsum(Itr begin, Itr end)
{
  typename std::iterator_traits<Itr>::value_type n;
  if (begin == end) {
    empty(n);
    return n;
  }
  assert(!isEmpty(*begin));
  n = exp(*begin);
  ++begin;
  for (; begin != end; ++begin) {
    assert(!isEmpty(*begin));
    n += exp(*begin);
  }
  return log(n);
}

template <typename Iterator>
inline
typename std::iterator_traits<Iterator>::value_type
expsum(std::pair<Iterator, Iterator> &p)
{
  return expsum(p.first, p.second);
}

template <typename T>
inline
T bitsum(T t)
{
  return t;
}

template <typename Itr>
inline
typename std::iterator_traits<Itr>::value_type bitsum(Itr begin, Itr end)
{
  typename std::iterator_traits<Itr>::value_type n;
  if (begin == end) {
    empty(n);
    return n;
  }
  assert(!isEmpty(*begin));
  n = pow (2,*begin);
  ++begin;
  for (; begin != end; ++begin) {
    assert(!isEmpty(*begin));
    n += pow(2,*begin);
  }
  return log(n) / log(2.0);
}

template <typename Iterator>
inline
typename std::iterator_traits<Iterator>::value_type
bitsum(std::pair<Iterator, Iterator> &p)
{
  return bitsum(p.first, p.second);
}


#include "list.hh"

#include "hash.hh"
#include "singleton.hh"
#include "asymptotics.hh"

/*
 * FIXME delete since second version is ok - only used 
 * for testing/prototype purpose, for real uses
 * classify-optimization with kbacktrack and hashtable
 * kicks in
 *
template <typename Iterator>
inline
List_Ref<typename std::iterator_traits<Iterator>::value_type>
unique(std::pair<Iterator, Iterator> &p)
{
  typedef typename std::iterator_traits<Iterator>::value_type type;
  Hash::Set<type> &set = Singleton<Hash::Set<type> >::ref();
  for (; p.first != p.second; ++p.first)
    set.add(*p.first);
  List_Ref<type> l;
  for (typename Hash::Set<type>::discard_iterator j = set.discard_begin();
       j!=set.discard_end(); ++j)
    move(l.ref().push_back_ref(), *j);
  set.reset();
  return l;
}
*/

template <typename Iterator>
inline
List_Ref<typename std::iterator_traits<Iterator>::value_type>
unique(std::pair<Iterator, Iterator> &p)
{
  typedef typename std::iterator_traits<Iterator>::value_type type;

  Hash::Set<type> set;
  for (; p.first != p.second; ++p.first)
    set.add(*p.first);
  set.finalize();
  List_Ref<type> l;
  for (typename Hash::Set<type>::iterator j = set.begin();
       j!=set.end(); ++j)
    move(l.ref().push_back_ref(), *j);

  return l;
}

template <typename Iterator>
inline
List_Ref<typename std::iterator_traits<Iterator>::value_type>
unique2(std::pair<Iterator, Iterator> &p)
{
  typedef typename std::iterator_traits<Iterator>::value_type type;
  List_Ref<type> ret;
  List<type> & l = ret.ref();
  for (; p.first != p.second; ++p.first) {
    bool add = true;
    for (typename List<type>::iterator i = l.begin(); i != l.end(); ++i)
      if (*i == *p.first) {
        add = false;
        break;
      }
    if (add)
      l.push_back(*p.first);
  }
  return ret;
}

template <typename Iterator>
inline
List_Ref<typename std::iterator_traits<Iterator>::value_type>
gt_zero(std::pair<Iterator, Iterator> &p)
{
  typedef typename std::iterator_traits<Iterator>::value_type type;
  List_Ref<type> l;
  for (Iterator i = p.first; i != p.second; ++i)
    if (*i > 0)
      push_back(l, *i);
  return l;
}

template <typename T>
inline
List_Ref<T> list(T t)
{
  List_Ref<T> r;
  if (is_not_empty(t))
    push_back(r, t);
  return r;
}


template <typename Iterator>
inline
List_Ref<typename std::iterator_traits<Iterator>::value_type>
xminimum(typename std::iterator_traits<Iterator>::value_type k, std::pair<Iterator, Iterator> &p)
{
  typedef typename std::iterator_traits<Iterator>::value_type type;
  List_Ref<type> l;
  for (; p.first != p.second; ++p.first)
    if (*p.first < k)
      push_back(l, *p.first);
  return l;
}

#ifdef USE_GSL

#include "sample.hh"

template <typename Iterator>
inline
typename std::iterator_traits<Iterator>::value_type
sample(std::pair<Iterator, Iterator> &p)
{
  Singleton<scil::ran_discrete>::ref().clear();
  for (; p.first != p.second; ++p.first)
    Singleton<scil::ran_discrete>::ref().push_back(*p.first);
  return Singleton<scil::ran_discrete>::ref().sample();
}

struct DoubleToDouble
{
  double operator()(double d) const
  {
    return d;
  }
};

template<typename S, typename T, typename pos_int, typename XToDouble>
inline
List_Ref<std::pair<S, T>, pos_int>
sample_filter(List_Ref<std::pair<S, T>, pos_int> &x,
    XToDouble todouble)
{
  List<std::pair<S, T>, pos_int> &l = x.ref();
  List_Ref<std::pair<S, T>, pos_int> ret;
  List<std::pair<S, T>, pos_int> &r = ret.ref();
  Singleton<scil::ran_discrete>::ref().clear();
  for (typename List<std::pair<S, T>, pos_int>::iterator i = l.begin();
       i != l.end(); ++i)
    Singleton<scil::ran_discrete>::ref().push_back(todouble((*i).first));
  Singleton<scil::ran_discrete>::ref().init();
  size_t s = Singleton<scil::ran_discrete>::ref().sample();
  for (typename List<std::pair<S, T>, pos_int>::iterator i = l.begin();
       i != l.end(); ++i, --s)
    if (!s) {
      r.push_back(*i);
      break;
    }
  assert(r.size() < 2);
  return ret;
}

template<typename S, typename T, typename pos_int>
inline
List_Ref<std::pair<S, T>, pos_int>
sample_filter(List_Ref<std::pair<S, T>, pos_int> &x)
{
  return sample_filter(x, DoubleToDouble() );
}

#endif

#endif
