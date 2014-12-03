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


#ifndef RANGE_HH
#define RANGE_HH

#include "list.hh"

template<typename T>
inline
std::pair<typename List<T>::iterator, typename List<T>::iterator>
get_range(List_Ref<T> &x)
{
  return std::pair<typename List<T>::iterator, typename List<T>::iterator>
    (x.ref().begin(), x.ref().end());
}

#include <functional>

template<typename Pair>
struct select1st : public std::unary_function<Pair,typename Pair::first_type> {
  const typename Pair::first_type & operator() (const Pair &p) const {
    return p.first;
  }
};

template<typename Pair>
struct select2nd : public std::unary_function<Pair,typename Pair::second_type> {
  const typename Pair::second_type & operator() (const Pair &p) const {
    return p.second;
  }
};

#include <iterator>

namespace Proxy {

template<typename Itr, typename Fn>
class Iterator {
  private:
    Itr curr;
  public:
    typedef typename Fn::result_type value_type;
    typedef typename Itr::difference_type difference_type;
    typedef typename Itr::iterator_category iterator_category;
    typedef typename Fn::result_type* pointer;
    typedef typename Fn::result_type& reference;

    Iterator(Itr i) : curr(i) {}

    Iterator &operator++()
    {
      ++curr;
      return *this;
    }

    const typename Fn::result_type &operator*()
    {
      return Fn()(*curr);
    }

    /*
    const typename Fn::result_type & operator*() const
    {
      return Fn()(*curr);
    }
    */

    bool operator==(const Iterator &other) const
    {
      return curr == other.curr;
    }

    bool operator!=(const Iterator &other) const
    {
      return !(*this == other);
    }
};

}

template <typename Iterator>
inline
std::pair<Proxy::Iterator<Iterator, select1st<typename Iterator::value_type> >,
  Proxy::Iterator<Iterator, select1st<typename Iterator::value_type> > >
splice_left(std::pair<Iterator, Iterator> p)
{
  typedef Proxy::Iterator<Iterator, select1st<typename Iterator::value_type> > foo;
  return std::make_pair(foo(p.first), foo(p.second));
}

template <typename Iterator>
inline
std::pair<Proxy::Iterator<Iterator, select2nd<typename Iterator::value_type> >,
  Proxy::Iterator<Iterator, select2nd<typename Iterator::value_type> > >
splice_right(std::pair<Iterator, Iterator> p)
{
  typedef Proxy::Iterator<Iterator, select2nd<typename Iterator::value_type> > foo;
  return std::make_pair(foo(p.first), foo(p.second));
}



#endif
