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

extern "C" {
  #include <immintrin.h>
}

#include <initializer_list>
#include <vector>
#include <limits>

#include "torch/extension.h"


/*
 * if all input Tensor have the same number of dimensions
 * and contain values of the same data type,
 * accessors can be created, which allow much faster element-wise
 * READ access to Tensor values e.g. in a loop;
 * this will be checked by GAPC when parsing the input<> information
 * in the GAP-L code;
 * if all input Tensors are the same, the macros "ALL_INPUT_TENSORS_SAME",
 * "INPUT_TENSOR_DIMS N" and "INPUT_TENSOR_TYPE DTYPE" will be defined,
 * which enable the "accessor" methods of the TensorChar and TensorSlice classes,
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
  // e.g. tensorslice.accessor()[i][j]
  torch::TensorAccessor<INPUT_TENSOR_TYPE, INPUT_TENSOR_DIMS> accessor() const {
    return t->accessor<INPUT_TENSOR_TYPE, INPUT_TENSOR_DIMS>();
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
  // e.g. tensorchar.accessor()[i][j]
  torch::TensorAccessor<INPUT_TENSOR_TYPE, INPUT_TENSOR_DIMS> accessor() const {
    return t->accessor<INPUT_TENSOR_TYPE, INPUT_TENSOR_DIMS>();
  }
#endif
};

#ifndef BATCHED_INPUT
// if batched input is processed, the global variable BATCH_SIZE will be
// set to the correct batch size of the input Tensors at runtime in out::init
inline int64_t BATCH_SIZE = 1;
inline int64_t count = 0;
#endif

#define MAX_BATCH_SIZE 256

#if defined(__AVX2__) && defined(BATCHED_INPUT)
/*
 * heavily optimized element-wise ops using 256bit registers;
 * the availabilty of these registers depends on the target CPU,
 * so this implementation will only be used if they are available;
 * if that's not the case, the vectorization of the loops will
 * be left up to the compiler/optimizer
 */

// define SIMD types and intrinsic functions based on NT table type
#if OUTPUT_CPP_TYPE == float
  #define register __m256
  #define LOOP_STEP 8
#elif OUTPUT_CPP_TYPE == double
  #define register __m256d
  #define LOOP_STEP 4
#elif OUTPUT_CPP_TYPE == BigInt
  #define register __m256i
  #define LOOP_STEP 4
#elif OUTPUT_CPP_TYPE == int
  #define register __m256i
  #define LOOP_STEP 8
#endif

#define ELEMENT_WISE_ON_BATCH(A, B, C, OPERATOR)
#define ELEMENT_WISE_ON_SCALAR(A, B, C, OPERATOR)

#else
/*
 * platform-independent, element-wise ops using a classic for loop;
 * these should automatically get vectorized by the compiler/optimizer,
 * but might not get compiled to optimal assembly;
 * for that reason, a manual AVX2 compatible implementation
 * is provided if AVX2/256bit registers are available
 */

#define ELEMENT_WISE_ON_BATCH(A, B, C, OPERATOR) \
for (int i = 0; i < BATCH_SIZE; ++i) { \
  C[i] = A[i] OPERATOR B[i]; \
}

#define ELEMENT_WISE_ON_SCALAR(A, SCALAR, C, OPERATOR) \
for (int i = 0; i < BATCH_SIZE; ++i) { \
  C[i] = A[i] OPERATOR SCALAR; \
}
#endif

/*
 * supports fast element-wise operations on batches of values;
 * the maximum batch size is defined by the MAX_BATCH_SIZE macro;
 * in the actual program, the batch size is determined at
 * runtime and stored in the global BATCH_SIZE variable,
 * which all objects have access to, so only BATCH_SIZE
 * elements are stored/processed in a Batch object
 */
template<typename T, int SIZE>
class Batch {
 public:
  bool empty_;
  T batch[SIZE];

  Batch() : empty_(false) {}

  // copy BATCH_SIZE elements from data into batch array
  explicit Batch(T *data) : empty_(false) {
    copy_from(data);
  }

  // fill batch array with BATCH_SIZE * x
  Batch(T x): empty_(false) {  // NOLINT [runtime/explicit]
    fill(x);
  }

  // copy constructor
  Batch(const Batch &other) {
    empty_ = other.empty_;
    if (!empty_) {
      copy_from(other.batch);
    }
  }

  // copy assignment operator
  Batch& operator=(const Batch &other) {
    empty_ = other.empty_;
    if (!empty_) {
      copy_from(other.batch);
    }
    return *this;
  }

  // move constructor
  Batch(Batch &&other) = default;

  // move assignment operator
  Batch& operator=(Batch &&other) = default;

  // fill batch array with BATCH_SIZE * x
  Batch& operator=(T x) {
    fill(x);
    return *this;
  }

  // fill batch array with Scalar value
  void fill(T x) {
    for (int i = 0; i < BATCH_SIZE; ++i) {
      batch[i] = x;
    }
  }

  // copy content of batch array into dest array
  void copy_to(T *dest) const {
    memcpy(dest, batch, BATCH_SIZE * sizeof(T));
  }

  // copy content from src array into batch array
  void copy_from(const T *src) {
    memcpy(batch, src, BATCH_SIZE * sizeof(T));
  }

  void empty() {
    empty_ = true;
  }

  bool isEmpty() const {
    return empty_;
  }

  T operator[](size_t i) const {
    return batch[i];
  }

  T& operator[](size_t i) {
    return batch[i];
  }

  // fast element-wise operations on the entire batch

  Batch operator+(T x) const {
    Batch res;
    ELEMENT_WISE_ON_SCALAR(batch, x, res, +)
    return res;
  }

  Batch& operator+=(T x) {
    ELEMENT_WISE_ON_SCALAR(batch, x, batch, +)
    return *this;
  }

  Batch operator-(T x) const {
    Batch res;
    ELEMENT_WISE_ON_SCALAR(batch, x, res, -)
    return res;
  }

  Batch& operator-=(T x) {
    ELEMENT_WISE_ON_SCALAR(batch, x, batch, -)
    return *this;
  }

  Batch operator*(T x) const {
    Batch res;
    ELEMENT_WISE_ON_SCALAR(batch, x, res, *)
    return res;
  }

  Batch& operator*=(T x) {
    ELEMENT_WISE_ON_SCALAR(batch, x, batch, *)
    return *this;
  }

  Batch operator/(T x) const {
    Batch res;
    ELEMENT_WISE_ON_SCALAR(batch, x, res, /)
    return res;
  }

  Batch& operator/=(T x) {
    ELEMENT_WISE_ON_SCALAR(batch, x, batch, /)
    return *this;
  }

  Batch operator+(const Batch &other) const {
    Batch res;
    ELEMENT_WISE_ON_BATCH(batch, other.batch, res, +)
    return res;
  }

  Batch& operator+=(const Batch &other) {
    ELEMENT_WISE_ON_BATCH(batch, other.batch, batch, +)
    return *this;
  }

  Batch operator-(const Batch &other) const {
    Batch res;
    ELEMENT_WISE_ON_BATCH(batch, other.batch, res, -)
    return res;
  }

  Batch& operator-=(const Batch &other) {
    ELEMENT_WISE_ON_BATCH(batch, other.batch, batch, -)
    return *this;
  }

  Batch operator*(const Batch &other) const {
    Batch res;
    ELEMENT_WISE_ON_BATCH(batch, other.batch, res, *)
    return res;
  }

  Batch& operator*=(const Batch &other) {
    ELEMENT_WISE_ON_BATCH(batch, other.batch, batch, *)
    return *this;
  }

  Batch operator/(const Batch &other) const {
    Batch res;
    ELEMENT_WISE_ON_BATCH(batch, other.batch, res, /)
    return res;
  }

  Batch& operator/=(const Batch &other) {
    ELEMENT_WISE_ON_BATCH(batch, other.batch, batch, /)
    return *this;
  }
};

// ### non-member operator overloads and Tensor ops ###

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline bool operator==(const Batch<T, SIZE> &lhs,
                       const Batch<T, SIZE> &rhs) {
  if (lhs.empty_ != rhs.empty_) {
    return false;
  }

  return true;
}

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline bool operator!=(const Batch<T, SIZE> &lhs,
                       const Batch<T, SIZE> &rhs) {
  return !(lhs == rhs);
}

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline void put(T *dest, const Batch<T, SIZE> &src) {
  if (src.empty_) {
    return;
  }

  src.copy_to(dest);
}

// multiply all values of "batch" with "true_scalar" where a == b,
// else multiply with "false_scalar" where a != b
template<typename T = float, int SIZE = MAX_BATCH_SIZE, typename SCALAR = float>
inline Batch<T, SIZE>
batched_multiply_if_else(const TensorChar &a, const TensorChar &b,
                         const Batch<T, SIZE> &x,
                         SCALAR true_scalar, SCALAR false_scalar) {
  Batch<T, SIZE> res;
  static auto a_ = a.accessor();
  static auto b_ = b.accessor();
  for (int64_t i = 0; i < BATCH_SIZE; ++i) {
    bool ans = a_[i][a.i] == b_[i][b.i];
    res[i] = x[i] * (true_scalar * ans + false_scalar * !ans);
  }

  return res;
}

// divide all values of "batch" by "true_scalar" where a == b,
// else divide by "false_scalar" where a != b
template<typename T = float, int SIZE = MAX_BATCH_SIZE, typename SCALAR = float>
inline Batch<T, SIZE>
batched_divide_if_else(const TensorChar &a, const TensorChar &b,
                         const Batch<T, SIZE> &x,
                         SCALAR true_scalar, SCALAR false_scalar) {
  Batch<T, SIZE> res;
  static auto a_ = a.accessor();
  static auto b_ = b.accessor();
  for (int64_t i = 0; i < BATCH_SIZE; ++i) {
    bool ans = a_[i][a.i] == b_[i][b.i];
    res[i] = x[i] / (true_scalar * ans + false_scalar * !ans);
  }

  return res;
}

// add "true_scalar" to all values of "batch" where a == b,
// else add "false_scalar" where a != b
template<typename T = float, int SIZE = MAX_BATCH_SIZE, typename SCALAR = float>
inline Batch<T, SIZE>
batched_add_if_else(const TensorChar &a, const TensorChar &b,
                         const Batch<T, SIZE> &x,
                         SCALAR true_scalar, SCALAR false_scalar) {
  Batch<T, SIZE> res;
  static auto a_ = a.accessor();
  static auto b_ = b.accessor();
  for (int64_t i = 0; i < BATCH_SIZE; ++i) {
    bool ans = a_[i][a.i] == b_[i][b.i];
    res[i] = x[i] + (true_scalar * ans + false_scalar * !ans);
  }

  return res;
}

// subtract "true_scalar" from all values of "batch" where a == b,
// else subtract "false_scalar" where a != b
template<typename T = float, int SIZE = MAX_BATCH_SIZE, typename SCALAR = float>
inline Batch<T, SIZE>
batched_subtract_if_else(const TensorChar &a, const TensorChar &b,
                         const Batch<T, SIZE> &x,
                         SCALAR true_scalar, SCALAR false_scalar) {
  Batch<T, SIZE> res;
  static auto a_ = a.accessor();
  static auto b_ = b.accessor();
  for (int64_t i = 0; i < BATCH_SIZE; ++i) {
    bool ans = a_[i][a.i] == b_[i][b.i];
    res[i] = x[i] - (true_scalar * ans + false_scalar * !ans);
  }

  return res;
}

// check if i-th column of the two compared tensors is equal
// (tensor_1[..., i].equal(tensor_2[..., i]))
inline bool equal(const TensorChar &lhs, const TensorChar &rhs) {
#if defined(ALL_INPUT_TENSORS_SAME) && INPUT_TENSOR_DIMS <= 2
  static auto lhs_ = lhs.accessor();
  static auto rhs_ = rhs.accessor();
  bool equal = true;

#if INPUT_TENSOR_DIMS == 2
  const int64_t n = lhs.t->sizes()[0];
  for (int64_t i = 0; i < n; ++i) {
    equal &= lhs_[i][lhs.i] == rhs_[i][rhs.i];
  }
#else
  equal = lhs_[lhs.i] == rhs_[rhs.i];
#endif

  return equal;
#else
  return torch::equal(lhs[{"...", lhs.i}], rhs[{"...", rhs.i}]);
#endif
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

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline void empty(Batch<T, SIZE> &t) {
  t.empty();
}

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline bool isEmpty(const Batch<T, SIZE> &t) {
  return t.isEmpty();
}

#endif  // RTLIB_TENSOR_HH_
