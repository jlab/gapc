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


#ifndef LIST_HH
#define LIST_HH

#include "empty.hh"
#include "erase.hh"

// FIXME profile this
#include "pool.hh"

// tr1 has it
#include <boost/cstdint.hpp>


#include <deque>

#include <new>
#include <cassert>

#include <iterator>
#include <algorithm>

//FIXME
//#include <iostream>

template<class T, typename pos_int>
struct List_Dummy;

template<class T, typename pos_int>
class List_Ref;

template <class T, typename pos_int = unsigned char>
class List : public std::deque<T> {};

#include <ostream>
#include "output.hh"

template<class T, typename pos_int>
inline
std::ostream &operator<<(std::ostream &out, const List<T, pos_int> &list)
{
  for (typename List<T, pos_int>::const_iterator i = list.begin(); i != list.end(); ++i)
    out << *i << '\n';
  return out;
}

#include "ref.hh"

template<class T, typename pos_int = unsigned char>
class List_Ref : public ::Ref::Lazy<List<T, pos_int> >
{
};

template<class T, typename pos_int>
inline
std::ostream &operator<<(std::ostream &out, const List_Ref<T, pos_int> &list)
{
  if (list.l)
    out << list.const_ref();
  return out;
}

template<class T, typename pos_int>
inline void empty(List_Ref<T, pos_int> &x)
{
  // empty as explicit initialization which is not needed with lists
  // could delete computed answers in previous alternatives ...
  //if (!isEmpty(x))
  //  x.ref().clear();
}

template<class T, typename pos_int>
inline bool isEmpty(const List_Ref<T, pos_int> &x)
{
  return !x.l || x.const_ref().empty();
}

template<class T, typename pos_int>
inline void erase(List_Ref<T, pos_int> &x)
{
    // TODO TG: why is this even here....
}

template<class T, typename pos_int>
inline void push_back(List_Ref<T, pos_int> &x, const T &e)
{
  assert(is_not_empty(e));
  x.ref().push_back(e);
}

template<class T, typename pos_int>
inline void append(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  
  std::copy(e.ref().begin(), e.ref().end(), std::back_inserter(x.ref())); 
}

template<class T, typename pos_int>
inline T get_front(List_Ref<T, pos_int> &x)
{
    return x.ref().front();
}

template<class T, typename pos_int>
inline void erase_element(List_Ref<T, pos_int> &x, typename List_Ref<T, pos_int>::iterator e)
{
  if (isEmpty(e))
    return;
  x.ref().erase(e);
}

#include "algebra.hh"

template<class T, typename pos_int>
inline List_Ref<T, pos_int> unique(List_Ref<T, pos_int> &x)
{
  typedef typename List_Ref<T, pos_int>::iterator itr;
  std::pair<itr, itr> p = std::make_pair(x->begin(), x->end());
  return unique(p);
}


#endif
