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
class List : public std::deque<T> {
    public:
        typedef typename std::deque<T>::reverse_iterator reverse_iterator;
};

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
    public:
        typedef typename List<T, pos_int>::reverse_iterator reverse_iterator;
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

// erase is meant as a destructor
// destructors on lists or auto called upon function end and therefore not needed
template<class T, typename pos_int>
inline void erase(List_Ref<T, pos_int> &x)
{   
}

// removes all elements from the list
template<class T, typename pos_int>
inline void clear(List_Ref<T, pos_int> &x)
{
    if(x.l) {
        x.ref().clear();
    }
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
inline T get_back(List_Ref<T, pos_int> &x)
{
    return x.ref().back();
}

template<class T, typename pos_int>
inline typename List_Ref<T, pos_int>::iterator erase_element(List_Ref<T, pos_int> &x, typename List_Ref<T, pos_int>::iterator e)
{
  return x.ref().erase(e);
}

template<class T, typename pos_int>
inline typename List_Ref<T, pos_int>::iterator insert_element(List_Ref<T, pos_int> &x, typename List_Ref<T, pos_int>::iterator e, T i)
{
  return x.ref().insert(e, i);
}



template<typename Iterator, typename Compare>
inline void sort_list(Iterator begin, Iterator end, Compare &c)
{
   std::sort(begin, end, c);
}

 


#include <iostream>
//#include <set>

#include "hash.hh"

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
    l.ref().push_back(*j);

  return l;
}



//#include <boost/unordered_set.hpp>
//
//template <typename Iterator>
//inline
//List_Ref<typename std::iterator_traits<Iterator>::value_type>
//unique(std::pair<Iterator, Iterator> &p)
//{
//  typedef typename std::iterator_traits<Iterator>::value_type type;
//
//  boost::unordered_set<type> set;
//  for (; p.first != p.second; ++p.first)
//    set.insert(*p.first);
//  //set.finalize();
//  List_Ref<type> l;
//  for (typename boost::unordered_set<type>::iterator j = set.begin();
//       j!=set.end(); ++j)
//    l.ref().push_back(*j);
//
//  return l;
//}

//template <typename Iterator>
//inline
//List_Ref<typename std::iterator_traits<Iterator>::value_type>
//unique(std::pair<Iterator, Iterator> &p)
//{
//  typedef typename std::iterator_traits<Iterator>::value_type type;
//
//  Hash::set<type> set;
//  for (; p.first != p.second; ++p.first)
//    set.insert(*p.first);
//  
//  List_Ref<type> l;
//  for (typename Hash::set<type>::iterator j = set.begin(); j!=set.end(); ++j)
//    l.ref().push_back(*j);
//
//  return l;
//}

#include "algebra.hh"

template<class T, typename pos_int>
inline List_Ref<T, pos_int> unique(List_Ref<T, pos_int> &x)
{
  typedef typename List_Ref<T, pos_int>::iterator itr;
  std::pair<itr, itr> p = std::make_pair(x.ref().begin(), x.ref().end());
  return unique(p);
}


#endif
