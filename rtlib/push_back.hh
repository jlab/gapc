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

#ifndef PUSH_BACK_HH
#define PUSH_BACK_HH

#include "list.hh"

template<typename T>
struct Left_Return
{
  typedef T type;
};


template<typename A, typename B>
struct Left_Return<std::pair<A, B> >
{
  typedef typename Left_Return<typename std::pair<A, B>::first_type>::type type;
};

template<class T>
inline T & left_most(T &e)
{
  return e;
}

template<typename A, typename B>
inline typename Left_Return<std::pair<A, B> >::type  & left_most(std::pair<A, B> &x)
{
  return left_most(x.first);
}

template<typename T>
struct Const_Left_Return
{
  typedef const T type;
};


template<typename A, typename B>
struct Const_Left_Return<const std::pair<A, B> >
{
  typedef
    const typename Left_Return<typename std::pair<A, B>::first_type>::type
    type;
};

template<class T>
inline const T & left_most(const T &e)
{
  return e;
}

template<typename A, typename B>
inline typename Const_Left_Return<const std::pair<A, B> >::type  & left_most(const std::pair<A, B> &x)
{
  return left_most(x.first);
}


template<class T, typename pos_int>
inline void push_back_max_other(List_Ref<T, pos_int> &x, T &e)
{
  assert(!isEmpty(e));
  if (isEmpty(x) || left_most(x.ref().front()) == left_most(e)) {
    x.ref().push_back(e);
    return;
  }
  if (left_most(x.ref().front()) < left_most(e)) {
    for (typename List<T, pos_int>::iterator i = x.ref().begin();
         i != x.ref().end(); ++i) {
      erase(*i);
    }
    x.ref().clear();
    x.ref().push_back(e);
    return;
  }
  erase(e);
}

template<class T, typename pos_int>
inline void push_back_min_other(List_Ref<T, pos_int> &x, T &e)
{
  assert(!isEmpty(e));
  if (isEmpty(x) || left_most(x.ref().front()) == left_most(e)) {
    x.ref().push_back(e);
    return;
  }
  if (left_most(x.ref().front()) > left_most(e)) {
    for (typename List<T, pos_int>::iterator i = x.ref().begin();
         i != x.ref().end(); ++i) {
      erase(*i);
    }
    x.ref().clear();
    x.ref().push_back(e);
    return;
  }
  erase(e);
}

// FIXME remove List_Ref versions of max/min/sum pushback/append

template<class T, typename pos_int>
inline void push_back_max(List_Ref<T, pos_int> &x, T &e)
{
  if (isEmpty(x)) {
    x.ref().push_back(e);
    return;
  }
  if (x.ref().front() < e) {
    erase(x.ref().front());
    x.ref().front() = e;
    return;
  }
  erase(e);
}

template<class T>
inline void push_back_max(T &x, T &e)
{
  if (isEmpty(x)) {
    x = e;
    return;
  }
  if (x < e) {
    x = e;
  }
}

template<class T, typename pos_int>
inline void push_back_min(List_Ref<T, pos_int> &x, T &e)
{
  if (isEmpty(x)) {
    x.ref().push_back(e);
    return;
  }
  if (x.ref().front() > e) {
    x.ref().front() = e;
    return;
  }
}

template<class T>
inline void push_back_min(T &x, T &e)
{
  if (isEmpty(x)) {
    x = e;
    return;
  }
  if (x > e) {
    x = e;
  }
}

template<class T, typename pos_int>
inline void push_back_sum(List_Ref<T, pos_int> &x, T &e)
{
  if (isEmpty(x)) {
    x.ref().push_back(e);
    return;
  }
  x.ref().front() += e;
}

template<class T>
inline void push_back_sum(T &x, T &e)
{
  if (isEmpty(x))
    x = e;
  else
    x += e;
}

template<class T, typename pos_int>
inline void append_max_other(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_max_other(x, *i);
}

template<class T, typename pos_int>
inline void append_min_other(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_min_other(x, *i);
}

template<class T, typename pos_int>
inline void append_max(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_max(x, *i);
}

template<class T>
inline void append_max(T &x, T &e)
{
  if (isEmpty(e))
    return;
  push_back_max(x, e);
}

template<class T, typename pos_int>
inline void append_min(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_min(x, *i);
}

template<class T>
inline void append_min(T &x, T &e)
{
  if (isEmpty(e))
    return;
  push_back_min(x, e);
}

template<class T, typename pos_int>
inline void append_sum(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_sum(x, *i);
}

template<class T>
inline void append_sum(T &x, T &e)
{
  if (isEmpty(e))
    return;
  push_back_sum(x, e);
}


template<class T, typename pos_int>
inline void push_back_class_syn(List_Ref<T, pos_int> &x, T &e)
{
  assert(!isEmpty(e));
  if (isEmpty(x)) {
    x.ref().push_back(e);
    return;
  }
  List<T, pos_int>  &l = x.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i) {
    if (left_most(*i) == left_most(e)) {
      (*i).second += e.second;
      return;
    }
  }
  l.push_back(e);
}

template<class T, typename pos_int>
inline void append_class_syn(List_Ref<T, pos_int> &x, List_Ref<T, pos_int> &e)
{
  if (isEmpty(e))
    return;
  assert(&x.ref() != &e.ref());
  List<T, pos_int> &l = e.ref();
  for (typename List<T, pos_int>::iterator i = l.begin(); i != l.end(); ++i)
    push_back_class_syn(x, *i);
}

#endif
