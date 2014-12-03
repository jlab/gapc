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


#ifndef EMPTY_HH
#define EMPTY_HH

#include <limits>
#include <cassert>
#include <cmath>
#include <utility>


template<typename T> inline void empty(T &x)
{
  x = 0;
}

template<typename T> inline bool isEmpty(const T &x)
{
  return x == 0;
}


//template<> inline void empty(int &x)
inline void empty(char &x)
{
  x = '~' + 1;
}

//template<> inline bool isEmpty(int x)
inline bool isEmpty(char x)
{
  return x == '~' + 1;
}

//template<> inline void empty(int &x)
inline void empty(int &x)
{
  x = std::numeric_limits<int>::max();
}

//template<> inline bool isEmpty(int x)
inline bool isEmpty(int x)
{
  return x == std::numeric_limits<int>::max();
}

//template<> inline void empty(double &x)
inline void empty(double &x)
{
  //assert( std::numeric_limits<double>::has_quiet_NaN );
  //x = std::numeric_limits<double>::quiet_NaN();
  assert( std::numeric_limits<double>::has_infinity );
  x = std::numeric_limits<double>::infinity();
}

//template<> inline bool isEmpty(double x)
inline bool isEmpty(double x)
{
  //assert( std::numeric_limits<double>::has_quiet_NaN );
  //return x != x;
  assert( std::numeric_limits<double>::has_infinity );
  return x == std::numeric_limits<double>::infinity();
}

inline void empty(float &x)
{
  assert( std::numeric_limits<float>::has_infinity );
  x = std::numeric_limits<float>::infinity();
}
inline bool isEmpty(float x)
{
  assert( std::numeric_limits<float>::has_infinity );
  return x == std::numeric_limits<float>::infinity();
}

// for void representation - see cpp.cc Type::Void ...
inline void empty(bool &x)
{
  x = false;
}

//template<> inline bool isEmpty(double x)
inline bool isEmpty(bool x)
{
  return !x;
}


#include "string.hh"

//template<> inline void empty(String &s)
inline void empty(String &s)
{
  s.empty();
}

//template<> inline bool isEmpty(const String &s)
inline bool isEmpty(const String &s)
{
  return s.isEmpty();
}

#include "rope.hh"

inline void empty(Rope &s)
{
  s.empty();
}

inline bool isEmpty(const Rope &s)
{
  return s.isEmpty();
}

template<typename T, typename U> inline void empty(std::pair<T, U> &p)
{
  empty(p.first);
  empty(p.second);
}

template <typename T, typename U>
inline bool isEmpty(const std::pair<T, U> &p)
{
  assert((isEmpty(p.first) || isEmpty(p.second))
         == (isEmpty(p.first) && isEmpty(p.second)));
  return isEmpty(p.first);
}

#include "subsequence.hh"

template<typename T, typename U> inline void empty(Basic_Subsequence<T, U> &p)
{
  p.empty();
}

template <typename T, typename U> inline bool isEmpty(const Basic_Subsequence<T, U> &p)
{
  return p.isEmpty();
}

#include "rope.hh"

template<typename X> inline void empty(rope::Ref<X> &p)
{
  p.empty();
}

template <typename X> inline bool isEmpty(const rope::Ref<X> &p)
{
  return p.isEmpty();
}

// FIXME this order is needed because of gcc resolution of dependent overloads

template<typename T> inline bool is_not_empty(const T &x)
{
  return !isEmpty(x);
}

// For multi-track:
template<typename T1, typename T2> inline bool is_not_empty(const T1 &x1, const T2 &x2)
{
  return !(isEmpty(x1) || isEmpty(x2));
}


#endif
