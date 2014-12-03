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

#ifndef RT_FILTER_HH
#define RT_FILTER_HH

#include "string.hh"
#include "sequence.hh"

#include <cassert>

template<typename alphabet, typename pos_type, typename T>
inline bool char_basepairing(const Basic_Sequence<alphabet, pos_type> &seq,
    T i, T j)
{
  if (j<=i+1)
    return false;
  char a = lower_case(seq[i]);
  char b = lower_case(seq[j-1]);

  switch (a) {
    case 'a' :
      switch (b) {
        case 'u' : return true;
        case 't' : return true;
      }
      break;
    case 'u' :
      switch (b) {
        case 'a' : return true;
        case 'g' : return true;
      }
      break;
    case 't' :
      switch (b) {
        case 'a' : return true;
      }
      break;
    case 'g' :
      switch (b) {
        case 'c' : return true;
        case 'u' : return true;
      }
      break;
    case 'c' :
      switch (b) {
        case 'g' : return true;
      }
      break;
  }
  return false;
}

template<typename alphabet, typename pos_type, typename T>
inline bool minsize(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j,
    int l)
{
  return j-i >= (pos_type) l;
}

template<typename alphabet, typename pos_type, typename T>
inline bool maxsize(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j,
    int l)
{
  return j-i <= (pos_type) l;
}


template<typename alphabet, typename pos_type, typename T>
inline bool equal(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j)
{
  if (j<=i+1)
    return false;
  return seq[i] == seq[j-1];
}

template<typename alphabet, typename pos_type, typename T>
inline bool all(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j)
{
  return true;
}

template<typename alphabet, typename pos_type, typename T>
inline bool onlychar(const Basic_Sequence<alphabet, pos_type> &seq,
                     T i, T j, alphabet x)
{
  if (j<i)
    return false;

  for (T k = i; k < j; k++) {
    if (seq[k] != x)
      return false;
  }
  return true;
}

template<typename a1, typename a2, typename pos_type, typename T>
inline bool samesize(const Basic_Sequence<a1, pos_type> &s1,
    T i1, T j1,
    T i2, T j2)
{
  assert(i1<=j1);
  assert(i2<=j2);
  return j1-i1 == j2-i2;
}

template<typename a1, typename pos_type, typename T>
inline bool samesize(const Basic_Sequence<a1, pos_type> &s1,
    T i1, T j1,
    T i3, T j3,
    T i2, T j2)
{
  assert(i1<=j1);
  assert(i2<=j2);
  return j1-i1 == j2-i2;
}


#endif
