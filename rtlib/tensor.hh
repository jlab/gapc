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
    * when generating a Pytorch module from GAPC derivative code;
    * 
    * there are two wrapper types:
    *   - TensorSlice (analogous to "Subsequence" type)
    *   - TensorChar  (analogous to "char" type)
    * 
    * these types are essentially drop-in replacements of "Subsequence"
    * and "char" in the generated Pytorch derivative code,
    * which takes Tensors as input instead of sequences and operates on Tensors;
    * 
    * this file also defines Tensor-related functions that can be used in the
    * GAP-L code to perform Tensor operations when operating on batched inputs;
}}} */

#ifndef RTLIB_TENSOR_HH_
#define RTLIB_TENSOR_HH_

#include <initializer_list>
#include <vector>
#include <limits>

#include "torch/extension.h"

#define DEFAULT_TORCH_TYPE torch::kFloat32
#define DEFAULT_CPP_TYPE   float

#ifndef BATCHED_INPUT
inline int64_t BATCH_SIZE = 1;
#endif

/*
 * if all input Tensor have the same number of dimensions
 * and contain values of the same data type,
 * accessors can be created, which allow much faster element-wise
 * READ access to Tensor values e.g. in a loop;
 * this will be checked by GAPC when parsing the input<> information
 * in the GAP-L code;
 * if all input Tensors are the same, the macros "ALL_INPUT_TENSORS_SAME",
 * "TENSOR_DIMS N" and "TENSOR_TYPE DTYPE" will be defined,
 * which enable the "at" methods of the TensorChar and TensorSlice classes,
 * granting access to their respective accessors;
 * these accessors require dimension- and datatype-information
 * at compile time, which is why they can only be defined
 * if all input Tensors share the same number of dimensions and dtype;
 * for Tensor-wide ops, the regular indexing methods of the TensorChar
 * and TensorSlice classes can be used without any concerns regarding speed
 */

using tensor = torch::Tensor;
using Slice = torch::indexing::Slice;  // Python's "Slice" (for indexing)
using tensoridx = std::initializer_list<at::indexing::TensorIndex>;
inline auto None = torch::indexing::None;  // Python's "None" (for indexing)

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
   * some usage examples (Python vs. C++):
   *   tensor[..., 1]  ==  TensorSlice[{"...", 1}]
   *   tensor[1:2]     ==  TensorSlice[Slice(1, 2)}]
   *   tensor[:, 1::2] ==  TensorSlice[{Slice(), Slice(1, None, 2)}]
   */
  tensor operator[](const tensoridx &idx) {
    return t->index(idx);
  }

  /*
   * pretty much identical to Python Tensor indexing;
   * some usage examples (Python vs. C++):
   *   tensor[..., 1]  ==  TensorSlice[{"...", 1}]
   *   tensor[1:2]     ==  TensorSlice[Slice(1, 2)}]
   *   tensor[:, 1::2] ==  TensorSlice[{Slice(), Slice(1, None, 2)}]
   */
  tensor operator[](const tensoridx &idx) const {
    return t->index(idx);
  }

  // ### basic tensor ops ###
  tensor operator+(const TensorSlice &other) {
    return (*this)[{"...", Slice(i, j)}] +
           other[{"...", Slice(other.i, other.j)}];
  }

  TensorSlice& operator+=(const TensorSlice &other) {
    (*this)[{"...", Slice(i, j)}] += other[{"...", Slice(other.i, other.j)}];
    return *this;
  }

  tensor operator-(const TensorSlice &other) {
    return (*this)[{"...", Slice(i, j)}] -
           other[{"...", Slice(other.i, other.j)}];
  }

  TensorSlice& operator-=(const TensorSlice &other) {
    (*this)[{"...", Slice(i, j)}] -= other[{"...", Slice(other.i, other.j)}];
    return *this;
  }

  tensor operator*(const TensorSlice &other) {
    return (*this)[{"...", Slice(i, j)}] *
           other[{"...", Slice(other.i, other.j)}];
  }

  TensorSlice& operator*=(const TensorSlice &other) {
    (*this)[{"...", Slice(i, j)}] *= other[{"...", Slice(other.i, other.j)}];
    return *this;
  }

  tensor operator/(const TensorSlice &other) {
    return (*this)[{"...", Slice(i, j)}] /
           other[{"...", Slice(other.i, other.j)}];
  }

  TensorSlice& operator/=(const TensorSlice &other) {
    (*this)[{"...", Slice(i, j)}] /= other[{"...", Slice(other.i, other.j)}];
    return *this;
  }

  // performs matrix multiplication of two TensorSlices
  tensor matmul(const TensorSlice &other) {
    return (*this)[{"...", Slice(i, j)}].mm(
             other[{"...", Slice(other.i, other.j)}]);
  }

#ifdef ALL_INPUT_TENSORS_SAME
  // use for efficient element-wise READ access in loops,
  // e.g. tensor.at()[i][j]
  torch::TensorAccessor<TENSOR_TYPE, TENSOR_DIMS>& at() {
    static auto accessor = t->accessor<TENSOR_TYPE, TENSOR_DIMS>();
    return accessor;
  }
#endif
};

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
   * some usage examples (Python vs. C++):
   *   tensor[..., 1]  ==  TensorSlice[{"...", 1}]
   *   tensor[1:2]     ==  TensorSlice[Slice(1, 2)}]
   *   tensor[:, 1::2] ==  TensorSlice[{Slice(), Slice(1, None, 2)}]
   */
  tensor operator[](const tensoridx &idx) {
    return t->index(idx);
  }

  /*
   * pretty much identical to Python Tensor indexing;
   * some usage examples (Python vs. C++):
   *   tensor[..., 1]  ==  TensorSlice[{"...", 1}]
   *   tensor[1:2]     ==  TensorSlice[Slice(1, 2)}]
   *   tensor[:, 1::2] ==  TensorSlice[{Slice(), Slice(1, None, 2)}]
   */
  tensor operator[](const tensoridx &idx) const {
    return t->index(idx);
  }

  // ### basic tensor ops ###

  tensor operator+(const TensorChar &other) {
    return (*this)[{"...", i}] + other[{"...", other.i}];
  }

  TensorChar& operator+=(const TensorChar &other) {
    (*this)[{"...", i}] += other[{"...", other.i}];
    return *this;
  }

  tensor operator-(const TensorChar &other) {
    return (*this)[{"...", i}] - other[{"...", other.i}];
  }

  TensorChar& operator-=(const TensorChar &other) {
    (*this)[{"...", i}] -= other[{"...", other.i}];
    return *this;
  }

  tensor operator*(const TensorChar &other) {
    return (*this)[{"...", i}] * other[{"...", other.i}];
  }

  TensorChar& operator*=(const TensorChar &other) {
    (*this)[{"...", i}] *= other[{"...", other.i}];
    return *this;
  }

  tensor operator/(const TensorChar &other) {
    return (*this)[{"...", i}] / other[{"...", other.i}];
  }

  TensorChar& operator/=(const TensorChar &other) {
    (*this)[{"...", i}] /= other[{"...", other.i}];
    return *this;
  }

  // calculates dot product of two TensorChars / columns of tensor
  tensor dot(const TensorChar &other) {
    return (*this)[{"...", i}].dot(other[{"...", other.i}]);
  }

#ifdef ALL_INPUT_TENSORS_SAME
  // use for efficient element-wise READ access in loops,
  // e.g. tensor.at()[i][j]
  torch::TensorAccessor<TENSOR_TYPE, TENSOR_DIMS>& at() {
    static auto accessor = t->accessor<TENSOR_TYPE, TENSOR_DIMS>();
    return accessor;
  }
#endif
};

// ### non-member operator overloads and Tensor ops ###

// check if i-th column of the two compared tensors is equal
// (tensor_1[..., i].equal(tensor_2[..., i]))
inline bool equal(const TensorChar &lhs, const TensorChar &rhs) {
  return torch::equal(lhs[{"...", lhs.i}], rhs[{"...", rhs.i}]);
}

// check if columns [i, j) of the two compared tensors are equal
// (tensor_1[..., i:j].equal(tensor_2[..., i:j]))
inline bool equal(const TensorSlice &lhs, const TensorSlice &rhs) {
  return torch::equal(lhs[{"...", Slice(lhs.i, lhs.j)}],
                      rhs[{"...", Slice(rhs.i, rhs.j)}]);
}

// element-wise == comparison for i-th column of the two compared tensors
inline tensor operator==(const TensorChar &lhs, const TensorChar &rhs) {
  return lhs[{"...", lhs.i}] == rhs[{"...", rhs.i}];
}

// element-wise != comparison for i-th column of the two compared tensors
inline tensor operator!=(const TensorChar &lhs, const TensorChar &rhs) {
  return ~(lhs == rhs);
}

// element-wise == comparison for columns [i, j) of the two compared tensors
inline tensor operator==(const TensorSlice &lhs, const TensorSlice &rhs) {
  return lhs[{"...", Slice(lhs.i, lhs.j)}] == rhs[{"...", Slice(rhs.i, rhs.j)}];
}

// element-wise != comparison for columns [i, j) of the two compared tensors
inline tensor operator!=(const TensorSlice &lhs, const TensorSlice &rhs) {
  return ~(lhs == rhs);
}

// calculates dot product of two TensorChars / columns of tensors
inline tensor dot(const TensorChar &lhs, const TensorChar &rhs) {
  return lhs[{"...", lhs.i}].dot(rhs[{"...", rhs.i}]);
}

// performs matrix multiplication of two TensorSlices
inline tensor matmul(const TensorSlice &lhs, const TensorSlice &rhs) {
  return lhs[{"...", Slice(lhs.i, lhs.j)}].mm(
           rhs[{"...", Slice(rhs.i, rhs.j)}]);
}

// create a 1D Tensor from a Scalar value (default size: 1)
template<typename T>
inline tensor tensor_from_scalar(T scalar, int size = 1) {
  return torch::full(size, scalar, torch::dtype(DEFAULT_TORCH_TYPE));
}

/*
 * add "true_val" to Tensor "t" at indices defined by boolean
 * index Tensor "condition";
 * "false_val" will be added to "t" at remaining indices;
 * "condition" Tensor can be created by e.g. performing element-
 * wise == comparisons on two Tensors (tensor_a == tensor_b)
 */
inline tensor add_if_else(const tensor &condition, tensor &t,
                          const torch::Scalar &true_val,
                          const torch::Scalar &false_val) {
  tensor flipped_condition = ~condition;
  t.index_put_({condition}, t.index({condition}) + true_val);
  t.index_put_({flipped_condition}, t.index({flipped_condition}) + false_val);
  return t;
}

/*
 * subtract "true_val" from Tensor "t" at indices defined by boolean
 * index Tensor "condition";
 * "false_val" will be subtracted from "t" at remaining indices;
 * "condition" Tensor can be created by e.g. performing element-
 * wise == comparisons on two Tensors (tensor_a == tensor_b)
 */
inline tensor subtract_if_else(const tensor &condition, tensor &t,
                               const torch::Scalar &true_val,
                               const torch::Scalar &false_val) {
  tensor flipped_condition = ~condition;
  t.index_put_({condition}, t.index({condition}) - true_val);
  t.index_put_({flipped_condition}, t.index({flipped_condition}) - false_val);
  return t;
}

/*
 * multiply "true_val" with Tensor "t" at indices defined by boolean
 * index Tensor "condition";
 * "false_val" will be multiplied with "t" at remaining indices;
 * "condition" Tensor can be created by e.g. performing element-
 * wise == comparisons on two Tensors (tensor_a == tensor_b)
 */
inline tensor multiply_if_else(const tensor &condition, tensor &t,
                               const torch::Scalar &true_val,
                               const torch::Scalar &false_val) {
  tensor flipped_condition = ~condition;
  t.index_put_({condition}, t.index({condition}) * true_val);
  t.index_put_({flipped_condition}, t.index({flipped_condition}) * false_val);
  return t;
}

/*
 * divide Tensor "t" by "true_val" at indices defined by boolean
 * index Tensor "condition";
 * "t" will be divided by "false_val" at remaining indices;
 * "condition" Tensor can be created by e.g. performing element-
 * wise == comparisons on two Tensors (tensor_a == tensor_b)
 */
inline tensor divide_if_else(const tensor &condition, tensor &t,
                             const torch::Scalar &true_val,
                             const torch::Scalar &false_val) {
  tensor flipped_condition = ~condition;
  t.index_put_({condition}, t.index({condition}) / true_val);
  t.index_put_({flipped_condition}, t.index({flipped_condition}) / false_val);
  return t;
}

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
  if (equal(curr_char, c)) {
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

inline void empty(tensor &t) {
  t = tensor_from_scalar(
        std::numeric_limits<DEFAULT_CPP_TYPE>::infinity(), BATCH_SIZE);
}

inline bool isEmpty(const tensor &t) {
  static const tensor EMPTY_TENSOR = tensor_from_scalar(
    std::numeric_limits<DEFAULT_CPP_TYPE>::infinity(), BATCH_SIZE);
  return torch::equal(t, EMPTY_TENSOR);
}

#endif  // RTLIB_TENSOR_HH_
