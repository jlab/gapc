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

    * author: fymue
    * contains wrapper types for torch::Tensor objects, which are used
    * when generating a pytorch module from GAPC derivative code;
    * there are two wrapper types:
    *   - TensorSlice (analogous to "Subsequence" type)
    *   - TensorChar  (analogous to "char" type)
    * these types are essentially drop-in replacements of "Subsequence"
    * and "char" in the generated code, which takes Tensors as input
    * instead of strings/sequences
}}} */

#ifndef RTLIB_TENSOR_HH_
#define RTLIB_TENSOR_HH_

#include "torch/extension.h"

using tensor = torch::Tensor;
using Slice = torch::indexing::Slice;

/*
 * represents a "Slice" of a Tensor with the shape [:, i:j],
 * (meaning the entire columns of the Tensor from [i, j))
 */
class TensorSlice {
 public:
  int64_t i, j;
  tensor *t;

  TensorSlice() : i(0), j(0), t(NULL) {}
  TensorSlice(tensor *t, int64_t i, int64_t j) : t(t), i(i), j(j) {}
  TensorSlice(const TensorSlice &other) : t(other.t), i(other.i), j(other.j) {}

  void empty() {
    t = NULL;
  }

  bool isEmpty() const {
    return !t;
  }

  int64_t size() const {
    return j - i;
  }
};

// check if tensor_1[:, i:j] == tensor_2[:, i:j]
inline bool operator==(const TensorSlice &lhs, const TensorSlice &rhs) {
  return torch::equal(lhs.t->index({"...", Slice(lhs.i, lhs.j)}),
                      rhs.t->index({"...", Slice(rhs.i, rhs.j)}));
}

// check if tensor_1[:, i:j] != tensor_2[:, i:j]
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

  TensorChar() : i(0), t(NULL) {}
  TensorChar(tensor *t, int64_t i) : t(t), i(i) {}
  TensorChar(const TensorChar &other) : t(other.t), i(other.i) {}

  void empty() {
    t = NULL;
  }

  bool isEmpty() const {
    return !t;
  }

  constexpr int64_t size() const {
    return 1;
  }
};

// check if tensor_1[:, i] == tensor_2[:, i]
inline bool operator==(const TensorChar &lhs, const TensorChar &rhs) {
  return torch::equal(lhs.t->index({"...", lhs.i}),
                      rhs.t->index({"...", rhs.i}));
}

// check if tensor_1[:, i] != tensor_2[:, i]
inline bool operator!=(const TensorChar &lhs, const TensorChar &rhs) {
  return !(lhs == rhs);
}

#endif  // RTLIB_TENSOR_HH_
