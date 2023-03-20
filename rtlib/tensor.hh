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

    * Author: fymue
    * wrapper types for Pytorch Tensor objects, which are used
    * when generating a pytorch module from GAPC derivative code;
    * there are two wrapper types:
    *   - TensorSlice (analogous to "Subsequence" type)
    *   - TensorChar  (analogous to "char" type)
    * these types are essentially drop-in replacements of "Subsequence"
    * and "char" in the generated Pytorch derivative code,
    * which takes Tensors as input instead of sequences and operates on Tensors
}}} */

#ifndef RTLIB_TENSOR_HH_
#define RTLIB_TENSOR_HH_

#include <initializer_list>
#include "torch/extension.h"

using tensor = torch::Tensor;
using Slice = torch::indexing::Slice;
using tensoridx = std::initializer_list<at::indexing::TensorIndex>;

/*
 * represents a "Slice" of a Tensor with the shape [:, i:j],
 * (meaning the entire columns of the Tensor from [i, j))
 */
class TensorSlice {
 public:
  int64_t i, j;
  tensor *t;

  TensorSlice() : i(0), j(0), t(nullptr) {}
  TensorSlice(tensor *t, int64_t i, int64_t j) : t(t), i(i), j(j) {}
  TensorSlice(TensorSlice &ts, int64_t i, int64_t j) : t(ts.t), i(i), j(j) {}
  TensorSlice(const TensorSlice &other) : t(other.t), i(other.i), j(other.j) {}

  void empty() {
    t = nullptr;
  }

  bool isEmpty() const {
    return !t;
  }

  int64_t size() const {
    return j - i;
  }

  /*
   * pretty much identical to Python Tensor indexing;
   * here we need to pass a initializer list though
   * to enable passing of multiple indices
   */
  tensor operator[](const tensoridx &idx) {
    return t->index(idx);
  }

  /*
   * pretty much identical to Python Tensor indexing;
   * here we need to pass a initializer list though
   * to enable passing of multiple indices
   */
  tensor operator[](const tensoridx &idx) const {
    return t->index(idx);
  }
};

// check if columns [i, j) of the two comared tensors are equal
// (tensor_1[..., i:j] == tensor_2[..., i:j])
inline bool operator==(const TensorSlice &lhs, const TensorSlice &rhs) {
  return torch::equal(lhs[{"...", Slice(lhs.i, lhs.j)}],
                      rhs[{"...", Slice(rhs.i, rhs.j)}]);
}

// check if columns [i, j) of the two comared tensors are not equal
// (tensor_1[..., i:j] != tensor_2[..., i:j])
inline bool operator!=(const TensorSlice &lhs, const TensorSlice &rhs) {
  return !(lhs == rhs);
}

/*
 * represents a "Char"/column of a Tensor
 */
class TensorChar {
 public:
  int64_t i;
  tensor *t;

  TensorChar() : i(0), t(nullptr) {}
  TensorChar(tensor *t, int64_t i) : t(t), i(i) {}
  TensorChar(TensorChar &tc, int64_t i) : t(tc.t), i(i) {}
  TensorChar(TensorSlice &ts, int64_t i) : t(ts.t), i(i) {}
  TensorChar(const TensorChar &other) : t(other.t), i(other.i) {}

  void empty() {
    t = nullptr;
  }

  bool isEmpty() const {
    return !t;
  }

  constexpr int64_t size() const {
    return 1;
  }

  /*
   * pretty much identical to Python Tensor indexing;
   * here we need to pass a initializer list though
   * to enable passing of multiple indices
   */
  tensor operator[](const tensoridx &idx) {
    return t->index(idx);
  }

  /*
   * pretty much identical to Python Tensor indexing;
   * here we need to pass a initializer list though
   * to enable passing of multiple indices
   */
  tensor operator[](const tensoridx &idx) const {
    return t->index(idx);
  }
};


// ### Tensor terminal parsers ###

template<typename T>
inline bool EMPTY(const TensorSlice &t, T i, T j) {
  return i == j;
}

// analogous to any terminal parser with Yield 1..n
// (parses Slice of input tensor)
template<typename T>
inline TensorSlice TSLICE(TensorSlice &t, T i, T j) {
  assert(i <= j);
  return TensorSlice(t, i, j);
}

// analogous to CHAR (parses column of input tensor)
template<typename T>
inline TensorChar TCHAR(TensorSlice &t, T i, T j, const TensorChar &c) {
  assert(i+1 == j);
  TensorChar curr_char(t, i);
  if (curr_char == c) {
    return c;
  } else {
    return TensorChar();  // default: empty
  }
}


// analogous to CHAR (parses column of input tensor)
template<typename T>
inline TensorChar TCHAR(TensorSlice &t, T i, T j) {
  assert(i+1 == j);
  return TensorChar(t, i);
}

// analogous to LOC
template<typename T>
inline TensorSlice TLOC(TensorSlice &t, T i, T j) {
  assert(i == j);
  return TensorSlice(t, i, j);
}

// ### non-member operator overloads for Tensors ###

// check if i-th column of the two comared tensors is equal
// (tensor_1[..., i] == tensor_2[..., i])
inline bool operator==(const TensorChar &lhs, const TensorChar &rhs) {
  return torch::equal(lhs[{"...", lhs.i}], rhs[{"...", rhs.i}]);
}

// check if i-th column of the two comared tensors is not equal
// (tensor_1[..., i] != tensor_2[..., i])
inline bool operator!=(const TensorChar &lhs, const TensorChar &rhs) {
  return !(lhs == rhs);
}

// ### empty.hh overloads ###

inline void empty(TensorChar &t) {
  t.empty();
}

inline bool isEmpty(const TensorChar &t) {
  return t.isEmpty();
}

inline void empty(TensorSlice &t) {
  t.empty();
}

inline bool isEmpty(const TensorSlice &t) {
  return t.isEmpty();
}

#endif  // RTLIB_TENSOR_HH_
