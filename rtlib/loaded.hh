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


#ifndef RTLIB_LOADED_HH_
#define RTLIB_LOADED_HH_

#include <limits>
#include <cassert>
#include <cmath>
#include <utility>

#include "subsequence.hh"
#include "string.hh"
#include "rope.hh"
#include "list.hh"
#include "ref.hh"

/*
  this header file contains functions to check whether or not
  a datatype is "loaded" or not. In this case, "loaded" means
  non-zero/non-empty (default constructed objects are 
  considered empty, for example), which needs to be detected 
  when loading checkpoints (--checkpoint option).
  This should work for all GAPC-generated/-supported types.
*/

inline bool is_loaded(char x) {
  return x != 0;
}

inline bool is_loaded(char *x) {
  return x != NULL;
}

inline bool is_loaded(int x) {
  return x != 0;
}

inline bool is_loaded(unsigned int x) {
  return x != 0;
}

inline bool is_loaded(int16_t x) {
  return x != 0;
}

inline bool is_loaded(int64_t x) {
  return x != 0;
}

inline bool is_loaded(float x) {
  return x != 0.0;
}

inline bool is_loaded(double x) {
  return x != 0.0;
}

inline bool is_loaded(long double x) {
  return x != 0.0;
}

inline bool is_loaded(bool x) {
  return x;
}

inline bool is_loaded(const String &s) {
  return s.isEmpty();
}

inline bool is_loaded(const Rope &s) {
  return s.size() > 0;
}

template <typename X> inline bool
is_loaded(const rope::Ref<X> &p) {
  return p.size() > 0;
}

template <typename T, typename U> inline bool
is_loaded(const std::pair<T, U> &p) {
  return is_loaded(p.first) || is_loaded(p.second);
}

template <typename T, typename U> inline bool
is_loaded(const Basic_Subsequence<T, U> &p) {
  return is_loaded(p.i) || is_loaded(p.j);
}

template <class T, typename pos_int> inline bool
is_loaded(const List_Ref<T, pos_int> &l) {
  return l.l && !(l.const_ref().empty());
}

template<class T, class I>
inline bool is_loaded(const Hash::Ref<T, I> &x) {
  return x.l && !(x.const_ref().isEmpty());
}

template <class T> inline bool
is_loaded(const Ref::Lazy<T> &l) {
  return l.l && !(l.const_ref().empty());
}

template <typename T> inline bool
is_loaded(const List<T> &l) {
  return !(l.empty());
}

// For multi-track
template<typename T1, typename T2> inline bool
is_loaded(const T1 &x1, const T2 &x2) {
  return is_loaded(x1) && is_loaded(x2);
}

#endif  // RTLIB_LOADED_HH_
