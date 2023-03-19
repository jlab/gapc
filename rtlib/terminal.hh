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


#ifndef RTLIB_TERMINAL_HH_
#define RTLIB_TERMINAL_HH_

#include <cassert>
#include <cstring>
#include <limits>
#include <cmath>

#include "empty.hh"
#include "string.hh"
#include "sequence.hh"
#include "subsequence.hh"



template<typename pos_type>
inline double CONST_FLOAT(Sequence &seq, pos_type i, pos_type j, double d) {
  assert(i == j);
  return d;
}

template<typename pos_type>
inline int CONST_INT(Sequence &seq, pos_type i, pos_type j, int d) {
  assert(i == j);
  return d;
}

template<typename pos_type>
inline char CONST_CHAR(Sequence &seq, pos_type i, pos_type j, char d) {
  assert(i == j);
  return d;
}

template<typename pos_type>
inline Rope CONST_ROPE(Sequence &seq, pos_type i, pos_type j, const char *d) {
  assert(i == j);
  Rope r;
  r.append(d);
  return r;
}

template<typename pos_type>
inline int INT(Sequence &seq, pos_type i, pos_type j) {
  assert(i < j);
  int result = 0;
  for (pos_type a = i; a < j; a++) {
    if (seq[a] < '0' || seq[a] > '9') {
      int r;
      empty(r);
      return r;
    }
    result = result * 10 + (seq[a] - '0');
  }
  return result;
}

template<typename pos_type>
inline float FLOAT(Sequence &seq, pos_type i, pos_type j) {
  assert(i < j);
  float result = 0;
  bool saw_exponent = false;
  int exponent = 0;

  // iterate through characters and return empty if char is neither . nor digit
  // if char is '.': start incrementing the exponent, i.e. count the number
  // of digits right of '.' and skip increasing result for now
  for (pos_type a = i; a < j; a++) {
    if (seq[a] == '.') {
      saw_exponent = true;
    } else {
      if (seq[a] < '0' || seq[a] > '9') {
        float r;
        empty(r);
        return r;
      }
      if (saw_exponent) {
        ++exponent;
      }
      result = result * 10 + (seq[a] - '0');
    }
  }

  // after all digits have been parsed and combined in one large int value
  // devide result by 10^exponent to get real float
  result = result / pow(10, exponent);
  return result;
}

template<typename alphabet, typename pos_type, typename T>
inline alphabet NON(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i+1 == j);
  double d = seq[i];
  if (d != d)
    return d;
  double r;
  empty(r);
  return r;
}

template<typename alphabet, typename pos_type, typename T, typename X>
inline alphabet CHAR(Basic_Sequence<alphabet, pos_type> &seq, T i, T j, X c) {
  assert(i+1 == j);
  if (seq[i] == c) {
    return c;
  } else {
    alphabet r;
    empty(r);
    return r;
  }
}

// template<typename Seq, typename pos_type>
// inline typename Seq::alphabet_type CHAR(Seq &seq, pos_type i, pos_type j)
template<typename alphabet, typename pos_type, typename T>
inline alphabet CHAR(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i+1 == j);
  return seq[i];
}

template <typename alphabet>
struct Sep {
};
template<>
struct Sep<char> {
  char sep() const { return '$'; }
};
template<>
struct Sep<double> {
  double sep() const {
    assert(std::numeric_limits<double>::has_quiet_NaN);
    return std::numeric_limits<double>::quiet_NaN();
  }
};
template<>
struct Sep<float> {
  float sep() const {
    assert(std::numeric_limits<float>::has_quiet_NaN);
    return std::numeric_limits<float>::quiet_NaN();
  }
};

template<typename alphabet, typename pos_type, typename T>
inline alphabet CHAR_SEP(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i+1 == j);
  Sep<alphabet> sep;
  if (seq[i] == sep.sep()) {
    alphabet r;
    empty(r);
    return r;
  } else {
    return seq[i];
  }
}


template<typename alphabet, typename pos_type, typename T>
inline bool EMPTY(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  return i == j;
}

// deprecated
template<typename alphabet, typename pos_type, typename T>
inline String STRING(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i < j);
  assert(0);
  // use ROPE
  return String();
}

template<typename alphabet, typename pos_type, typename T>
inline Rope ROPE(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i < j);
  Rope r;
  r.append(seq.begin() + i, j-i);
  return r;
}

/* a ROPE terminal parser that accepts an argument that restricts parse to
   this pattern
   for example ROPE("stefan") would only yield if sub-word contains "stefan" */
template<typename alphabet, typename pos_type, typename T, typename X>
inline Rope ROPE(Basic_Sequence<alphabet, pos_type> &seq, T i, T j, X pattern) {
  assert(i+strlen(pattern) == j);
  Rope res;
  pos_type pos_pattern = 0;
  for (pos_type a = i; a < j; a++, pos_pattern++) {
    if (seq[a] != pattern[pos_pattern]) {
      Rope r;
      empty(r);
      return r;
    }
    append(res, seq[a]);
  }
  return res;
}

template<typename alphabet, typename pos_type, typename T>
inline Rope ROPE0(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i <= j);
  Rope r;
  r.append(seq.begin() + i, j-i);
  return r;
}

template<typename alphabet, typename pos_type, typename T>
inline Basic_Subsequence<alphabet, pos_type>
REGION(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i < j);
  return Basic_Subsequence<alphabet, pos_type>(seq, i, j);
}


// XXX deprecated
template<typename alphabet, typename pos_type, typename T>
inline Basic_Subsequence<alphabet, pos_type>
UREGION(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i <= j);
  return Basic_Subsequence<alphabet, pos_type>(seq, i, j);
}

template<typename alphabet, typename pos_type, typename T>
inline Basic_Subsequence<alphabet, pos_type>
REGION0(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i <= j);
  return Basic_Subsequence<alphabet, pos_type>(seq, i, j);
}


template<typename alphabet, typename pos_type, typename T>
inline Basic_Subsequence<alphabet, pos_type>
BASE(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i+1 == j);
  return Basic_Subsequence<alphabet, pos_type>(seq, i, j);
}

template<typename alphabet, typename pos_type, typename T>
inline Basic_Subsequence<alphabet, pos_type>
LOC(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i == j);
  return Basic_Subsequence<alphabet, pos_type>(seq, i, j);
}

// Needed for applications where only some sort of sub-string
// placeholder is needed: e.g. if answer of a string terminal
// parser is not used or only its length
// eliminates unneeded temporary string creation
// for example in adpfold or local alignment

template<typename alphabet, typename pos_type, typename T>
inline int SEQ(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i <= j);
  return j - i;
}

template<typename alphabet, typename pos_type, typename T>
inline int SEQ1(Basic_Sequence<alphabet, pos_type> &seq, T i, T j) {
  assert(i < j);
  return j - i;
}

#ifdef PYTORCH_MOD
using Slice = torch::indexing::Slice;
// tensor == torch::Tensor

template<typename T>
inline bool EMPTY(tensor &t, T i, T j) {
  return i == j;
}

template<typename T>
inline tensor& TSLICE(tensor &t, T i, T j) {
  assert(i == j);
  return t.index({"...", Slice(i, j)});  // same as np.ndarray[:, i:j]
}

template<typename T>
inline tensor& TCHAR(const tensor &t, T i, T j, tensor &c) {
  assert(i+1 == j);
  if (torch::equal(t.index({"...", i}), c)) {
    return c;
  } else {
    return torch::zeros(0):
  }
}

template<typename T>
inline tensor& TCHAR(const tensor &t, T i, T j) {
  assert(i+1 == j);
  return t.index({"...", i});  // same as np.ndarray[:, i]
}

template<typename T>
inline tensor& TLOC(const tensor &t, T i, T j) {
  assert(i == j);
  return torch::zeros(0);
}
#endif


#endif  // RTLIB_TERMINAL_HH_
