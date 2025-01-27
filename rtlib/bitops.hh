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

#ifndef RTLIB_BITOPS_HH_
#define RTLIB_BITOPS_HH_

#include <cassert>

#include <boost/cstdint.hpp>

template <typename T>
int
inline
slow_ffs(T t) {
  for (unsigned int i = 0; i < sizeof(T) * 8; ++i)
    if (t & 1 << i)
      return i+1;
  return 0;
}

int
inline
find_first_set(uint64_t t) {
#ifdef __GNUC__
  assert(sizeof(uint64_t) == 8);
  return __builtin_ffsll(t);
#else
  return slow_ffs(t);
#endif
}

int
inline
find_first_set(uint32_t t) {
#ifdef __GNUC__
  assert(sizeof(unsigned int) == 4);
  return __builtin_ffs(t);
#else
  return slow_ffs(t);
#endif
}

template<typename T>
inline
int
find_first_set(T t) {
  return slow_ffs(t);
}

// //

template <typename T>
int
inline
slow_clz(T t) {
  for (unsigned int i = sizeof(T) * 8; i > 0; --i)
    if (t & T(1) << i-1)
      return sizeof(T)*8 - i;
  assert(false);
  return 23;
}

int
inline
count_leading_zeroes(uint64_t t) {
  assert(t);
#ifdef __GNUC__
  assert(sizeof(uint64_t) == 8);
  return __builtin_clzll(t);
#else
  return slow_clz(t);
#endif
}

int
inline
count_leading_zeroes(uint32_t t) {
  assert(t);
#ifdef __GNUC__
  assert(sizeof(unsigned int) == 4);
  return __builtin_clz(t);
#else
  return slow_clz(t);
#endif
}

template<typename T>
inline
int
count_leading_zeroes(T t) {
  assert(t);
  return slow_clz(t);
}

// //

template<typename T>
inline
T
size_to_next_power(T t) {
  assert(t);
  assert(t <= (T(1) << ((sizeof(T)*8)-1)));
  T ret = T(1) << (((sizeof(T)*8)-1)-count_leading_zeroes(t));
  if (ret < t)
    return 2*ret;
  else
    return ret;
}

// //
#include "shape_alph.hh"

namespace hash_to_uint32 {
struct djb_slow {
  uint32_t initial() const { return 5381; }

  void next(uint32_t &hash, uint64_t t) const {
    hash = hash * 33 + static_cast<uint32_t>(t);
    hash = hash * 33 + static_cast<uint32_t>(t>>32);
  }
};

struct djb {
  uint32_t initial() const { return 5381; }

  void next(uint32_t &hash, uint64_t t) const {
    hash = ((hash << 5) + hash) + static_cast<uint32_t>(t);
    hash = ((hash << 5) + hash) + static_cast<uint32_t>(t>>32);
  }

  void next(uint32_t &hash, uint32_t t) const {
    hash = ((hash << 5) + hash) + t;
  }

  void next(uint32_t &hash, char t) const {
    hash = ((hash << 5) + hash) + static_cast<uint32_t>(t);
  }
};

struct djb_chars {
  uint32_t initial() const { return 5381; }

  void next(uint32_t &hash, uint64_t t) const {
    ShapeAlph<uint64_t, unsigned char> alph;
    uint64_t *x = &t;
    for (;;) {
      for (unsigned char i = 64; i > 40; i -= 2) {
        char c = alph.to_char(*x, i-2);
        if (c)
          hash = hash * 33 + c;
        else
          break;
      }
      break;
      //
      if (*x & static_cast<uint64_t>(-1)  >> (64-2))
        ++x;
      else
        break;
    }
  }
};

struct sdbm {
  uint32_t initial() const { return 0; }

  void next(uint32_t &hash, uint64_t t) const {
    hash = static_cast<uint32_t>(t) + (hash << 6) + (hash << 16) - hash;
    hash = static_cast<uint32_t>(t >> 32) + (hash << 6) + (hash << 16) - hash;
  }
  template<typename T>
  void next(uint32_t &hash, T t) const {
    hash = t + (hash << 6) + (hash << 16) - hash;
  }
};
}  // namespace hash_to_uint32


#endif  // RTLIB_BITOPS_HH_
