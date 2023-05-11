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
    * this file contains an implementation of a "Batch" object,
    * which supports fast element-wise operations on batches
    * of primitive values;
    * this type is used in the generated derivative code
    * to process batched inputs;
    * it can be used as an answer type in GAP-L algebra functions
    * using the typenames
    * "I32batch", "I64batch", "F32batch" or "F64batch";
    * it uses a memory pool for fast (de)allocation of memory;
    * all "Batch" objects require access to a global variable
    * "BATCH_SIZE", which determines the size of every batch
    * (gets set at runtime in the generated derivative code);
    * the maximum batch size is controlled by the "MAX_BATCH_SIZE"
    * macro, which is used to allocate space for every batch
    * from the memory pool and thus needs to be a fixed size;
    * by default, it is set to 512, but can be
    * increased/decreased if needed since the memory pool
    * can automatically expand itself
}}} */

#ifndef RTLIB_BATCH_HH_
#define RTLIB_BATCH_HH_

extern "C" {
  #include <immintrin.h>
}

#include <cstdint>
#include <cstring>
#include "pool.hh"
#include "empty.hh"

#ifndef BATCHED_INPUT
// if batched input is processed, the global variable BATCH_SIZE will be
// set to the correct batch size of the input Tensors at runtime in out::init
inline int64_t BATCH_SIZE = 1;
#endif

#define MAX_BATCH_SIZE 512

#if defined(__AVX2__)
/*
 * SIMD element-wise operations on batches using 256bit registers;
 * the availabilty of these registers depends on the target CPU,
 * so this implementation will only be used if they are available;
 * if that's not the case, the vectorization of the loops will
 * be left up to the compiler/optimizer
 */

#if __OUTPUT_CPP_TYPE == FLOAT_TYPE             // 32bit float (F32)
  #define register         __m256               // 256bit register for F32's
  #define LOOP_STEP        8                    // register holds 8 F32's
  #define ADD              _mm256_add_ps        // add
  #define SUB              _mm256_sub_ps        // subtract
  #define MUL              _mm256_mul_ps        // multiply
  #define DIV              _mm256_div_ps        // divide
  #define FILL_WITH_SCALAR _mm256_set1_ps       // fill register with F32 scalar
  #define LOAD             _mm256_loadu_ps      // load 8 F32's from ptr
  #define STORE            _mm256_storeu_ps     // store 8 F32's at ptr

#elif __OUTPUT_CPP_TYPE == DOUBLE_TYPE          // 64bit float (F64)
  #define register         __m256d              // 256bit register for F64's
  #define LOOP_STEP        4                    // register holds 4 F64's
  #define ADD              _mm256_add_pd        // add
  #define SUB              _mm256_sub_pd        // subtract
  #define MUL              _mm256_mul_pd        // multiply
  #define DIV              _mm256_div_pd        // divide
  #define FILL_WITH_SCALAR _mm256_set1_pd       // fill register with F64 scalar
  #define LOAD             _mm256_loadu_pd      // load 4 F64's from ptr
  #define STORE            _mm256_storeu_pd     // store 4 F64's at ptr

#elif __OUTPUT_CPP_TYPE == INT_TYPE             // 32bit integer (I32)
  #define register         __m256i              // 256bit register for I32's
  #define LOOP_STEP        8                    // register holds 8 I32's
  #define ADD              _mm256_add_epi32     // add
  #define SUB              _mm256_sub_epi32     // subtract
  #define MUL              _mm256_mul_epi32     // multiply
  #define FILL_WITH_SCALAR _mm256_set1_epi32    // fill register with I32 scalar
  #define LOAD             _mm256_loadu_epi32   // load 8 I32's from ptr
  #define STORE            _mm256_storeu_epi32  // store 8 I32's at ptr

#elif __OUTPUT_CPP_TYPE == BIGINT_TYPE          // 64bit integer (I64)
  #define register         __m256i              // 256bit register for I64's
  #define LOOP_STEP        4                    // register holds 4 I64's
  #define ADD              _mm256_add_epi64     // add
  #define SUB              _mm256_sub_epi64     // subtract
  #define MUL              _mm256_mul_epi64     // multiply
  #define FILL_WITH_SCALAR _mm256_set1_epi64    // fill register with I64 scalar
  #define LOAD             _mm256_loadu_epi64   // load 4 I64's from ptr
  #define STORE            _mm256_storeu_epi64  // store 4 I64's at ptr

#endif

#define ELEMENT_WISE_ON_BATCH(A, B, C, OPERATOR)       \
register a, b, c;                                      \
for (int i = 0; i < BATCH_SIZE; i += LOOP_STEP) {      \
  a = LOAD(A + i);                                     \
  b = LOAD(B + i);                                     \
  c = OPERATOR(a, b);                                  \
  STORE(C + i, c);                                     \
}

#define ELEMENT_WISE_ON_SCALAR(A, SCALAR, C, OPERATOR) \
register b = FILL_WITH_SCALAR(SCALAR);                 \
register a, c;                                         \
for (int i = 0; i < BATCH_SIZE; i += LOOP_STEP) {      \
  a = LOAD(A + i);                                     \
  c = OPERATOR(a, b);                                  \
  STORE(C + i, c);                                     \
}

#endif

#if !defined(__AVX2__) || !defined(DIV)
/*
 * platform-independent, element-wise operations using a classic for loop;
 * these should automatically get vectorized by the compiler/optimizer,
 * but might not get compiled to optimal assembly;
 * for that reason, a manual AVX2 compatible implementation
 * is provided if AVX2/256bit registers are available (see above);
 * However, if the "DIV" macro is undefined, the functions below
 * will be used over their AVX2 analogs, even if AVX2 is available;
 * This is because there is no AVX2 SIMD division instruction for integers,
 * so the AVX2 functions can't be used if two integer batches are supposed
 * to be divided element-wise, so we will just leave it up to
 * the compiler to optimize/vectorize the loops in this case
 */

#define ADD +
#define SUB -
#define MUL *
#define DIV /

#define ELEMENT_WISE_ON_BATCH(A, B, C, OPERATOR)       \
for (int i = 0; i < BATCH_SIZE; ++i) {                 \
  C[i] = A[i] OPERATOR B[i];                           \
}

#define ELEMENT_WISE_ON_SCALAR(A, SCALAR, C, OPERATOR) \
for (int i = 0; i < BATCH_SIZE; ++i) {                 \
  C[i] = A[i] OPERATOR SCALAR;                         \
}
#endif

/*
 * contains the data of a "Batch object";
 * this object gets allocated in a memory pool,
 * so it's "new" and "delete"
 * operators need to be overloaded;
 */
template<typename T, int SIZE>
class BatchImpl {
 public:
  uint32_t ref_count;
  T data[SIZE];

  BatchImpl() : ref_count(1) {}

  void inc_ref() {
    ++ref_count;
  }

  void dec_ref() {
    assert(ref_count > 0);
    --ref_count;
  }

  void *operator new(size_t t) noexcept(false);

  void operator delete(void *b) noexcept(false);
};

/*
 * supports fast element-wise operations on batches of values;
 * the maximum batch size is defined by the MAX_BATCH_SIZE macro;
 * in the generated derivative code, the batch size is determined at
 * runtime and stored in the global BATCH_SIZE variable,
 * which all objects have access to; so only BATCH_SIZE
 * elements are stored/processed in a Batch object
 */
template<typename T, int SIZE>
class Batch {
 private:
  T _empty_val;

 public:
  static Pool<BatchImpl<T, SIZE>> pool;
  bool empty_;
  BatchImpl<T, SIZE> *batch;

  Batch() : empty_(false), batch(nullptr) {
    empty(_empty_val);
  }

  // copy BATCH_SIZE elements from data into batch array
  explicit Batch(T *data) : empty_(false), batch(nullptr) {
    empty(_empty_val);
    copy_from(data);
  }

  // fill batch array with scalar value
  Batch(T x): empty_(false), batch(nullptr) {  // NOLINT [runtime/explicit]
    empty(_empty_val);
    fill(x);
  }

  ~Batch() {
    if (!batch) {
      return;
    }
    assert(batch->ref_count);
    batch->dec_ref();
    if (!batch->ref_count) {
      delete batch;
      batch = nullptr;
    }
  }

  // copy constructor
  Batch(const Batch &other) {
    _empty_val = other._empty_val;
    empty_ = other.empty_;
    batch = other.batch;
    if (batch) {
      batch->inc_ref();
    }
  }

  // copy assignment operator
  Batch& operator=(const Batch &other) {
    empty_ = other.empty_;
    batch = other.batch;
    if (batch) {
      batch->inc_ref();
    }
    return *this;
  }

  // fill batch array with scalar value
  Batch& operator=(T x) {
    fill(x);
    return *this;
  }

  // allocate memory for batch array from the memory pool
  void alloc() {
    if (!batch) {
      batch = new BatchImpl<T, SIZE>();
    }
  }

  // fill batch array with scalar value
  void fill(T x) {
    alloc();
    assert(batch->data);
    for (int i = 0; i < BATCH_SIZE; ++i) {
      batch->data[i] = x;
    }
  }

  // copy content of batch array into dest array
  void copy_to(T *dest) const {
    assert(batch->data);
    memcpy(dest, batch->data, BATCH_SIZE * sizeof(T));
  }

  // copy content from src array into batch array
  void copy_from(const T *src) {
    alloc();
    assert(batch->data);
    memcpy(batch->data, src, BATCH_SIZE * sizeof(T));
  }

  void _empty() {
    // fill batch with "empty" T val
    fill(_empty_val);
    empty_ = true;
  }

  bool isEmpty() const {
    return empty_;
  }

  T operator[](size_t i) const {
    return batch->data[i];
  }

  T& operator[](size_t i) {
    return batch->data[i];
  }

  // fast element-wise operations on the entire batch

  Batch operator+(T x) const {
    Batch res;
    res.alloc();
    ELEMENT_WISE_ON_SCALAR(batch->data, x, res.batch->data, ADD)
    return res;
  }

  Batch& operator+=(T x) {
    ELEMENT_WISE_ON_SCALAR(batch->data, x, batch->data, ADD)
    return *this;
  }

  Batch operator-(T x) const {
    Batch res;
    res.alloc();
    ELEMENT_WISE_ON_SCALAR(batch->data, x, res.batch->data, SUB)
    return res;
  }

  Batch& operator-=(T x) {
    ELEMENT_WISE_ON_SCALAR(batch->data, x, batch->data, SUB)
    return *this;
  }

  Batch operator*(T x) const {
    Batch res;
    res.alloc();
    ELEMENT_WISE_ON_SCALAR(batch->data, x, res.batch->data, MUL)
    return res;
  }

  Batch& operator*=(T x) {
    ELEMENT_WISE_ON_SCALAR(batch->data, x, batch->data, MUL)
    return *this;
  }

  Batch operator/(T x) const {
    Batch res;
    res.alloc();
    ELEMENT_WISE_ON_SCALAR(batch->data, x, res.batch->data, DIV)
    return res;
  }

  Batch& operator/=(T x) {
    ELEMENT_WISE_ON_SCALAR(batch->data, x, batch->data, DIV)
    return *this;
  }

  Batch operator+(const Batch &other) const {
    Batch res;
    res.alloc();
    ELEMENT_WISE_ON_BATCH(batch->data, other.batch->data, res.batch->data, ADD)
    return res;
  }

  Batch& operator+=(const Batch &other) {
    ELEMENT_WISE_ON_BATCH(batch->data, other.batch->data, batch->data, ADD)
    return *this;
  }

  Batch operator-(const Batch &other) const {
    Batch res;
    res.alloc();
    ELEMENT_WISE_ON_BATCH(batch->data, other.batch->data, res.batch->data, SUB)
    return res;
  }

  Batch& operator-=(const Batch &other) {
    ELEMENT_WISE_ON_BATCH(batch->data, other.batch->data, batch->data, SUB)
    return *this;
  }

  Batch operator*(const Batch &other) const {
    Batch res;
    res.alloc();
    ELEMENT_WISE_ON_BATCH(batch->data, other.batch->data, res.batch->data, MUL)
    return res;
  }

  Batch& operator*=(const Batch &other) {
    ELEMENT_WISE_ON_BATCH(batch->data, other.batch->data, batch->data, MUL)
    return *this;
  }

  Batch operator/(const Batch &other) const {
    Batch res;
    res.alloc();
    ELEMENT_WISE_ON_BATCH(batch->data, other.batch->data, res.batch->data, DIV)
    return res;
  }

  Batch& operator/=(const Batch &other) {
    ELEMENT_WISE_ON_BATCH(batch->data, other.batch->data, batch->data, DIV)
    return *this;
  }
};

// ### new and delete overloads for BatchImpl objects ###

template<typename T, int SIZE>
Pool<BatchImpl<T, SIZE>> Batch<T, SIZE>::pool;

template<typename T, int SIZE>
void* BatchImpl<T, SIZE>::operator new(size_t t) noexcept(false) {
  assert(t == sizeof(BatchImpl<T, SIZE>));
  BatchImpl<T, SIZE> *r = Batch<T, SIZE>::pool.malloc();
  return r;
}

template<typename T, int SIZE>
void BatchImpl<T, SIZE>::operator delete(void *b) noexcept(false) {
  if (!b) {
    return;
  }
  Batch<T, SIZE>::pool.free(static_cast<BatchImpl<T, SIZE>*>(b));
}

// ### non-member operator/function overloads ###

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline bool operator==(const Batch<T, SIZE> &lhs,
                       const Batch<T, SIZE> &rhs) {
  if (lhs.empty_ != rhs.empty_) {
    return false;
  }

  bool equal = true;
  for (int i = 0; i < BATCH_SIZE; ++i) {
    equal &= lhs[i] == rhs[i];
  }

  return equal;
}

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline bool operator!=(const Batch<T, SIZE> &lhs,
                       const Batch<T, SIZE> &rhs) {
  return !(lhs == rhs);
}

// copy content of batch into dest array
template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline void put(T *dest, const Batch<T, SIZE> &src) {
  if (src.empty_) {
    return;
  }

  src.copy_to(dest);
}

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline void empty(Batch<T, SIZE> &t) {
  t._empty();
}

template<typename T = float, int SIZE = MAX_BATCH_SIZE>
inline bool isEmpty(const Batch<T, SIZE> &t) {
  return t.isEmpty();
}

#endif  // RTLIB_BATCH_HH_
