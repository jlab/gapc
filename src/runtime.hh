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


#ifndef SRC_RUNTIME_HH_
#define SRC_RUNTIME_HH_

#include <iostream>
#include <vector>
#include <cassert>

#include "yieldsize.hh"
#include "table.hh"

// tr1 has it
#include <boost/cstdint.hpp>


#ifndef UINT32_MAX
#define UINT32_MAX 4294967295U
#endif


// see also http://www.fefe.de/intof.html
inline bool mult_uint32_t(uint32_t a, uint32_t b, uint32_t &c) {
  /* Update 20150422: clang and gcc 5 have builtins now for this, look for
     __builtin_add_overflow and __builtin_mul_overflow in the documentation.
    That makes these macros obsolete. */
  return !__builtin_mul_overflow(a, b, &c);
}


namespace Runtime {
class Poly;

namespace Asm {
class Poly {
  friend class Runtime::Poly;

 private:
  uint32_t n;

 public:
  Poly() : n(0) { }

  explicit Poly(uint32_t term) : n(term+1) {
  }

  Poly(uint32_t a, uint32_t b) : n(b+1) {
  }

  explicit Poly(const Table &t) {
    assert(t.type() != Table::NONE);
    n = 1;
    switch (t.type()) {
    case Table::LINEAR :
      n = 2; break;
    case Table::QUADRATIC :
      n = 3; break;
    default:
      break;
    }
  }

  explicit Poly(const Yield::Poly &y) {
    if (y == Yield::UP) {
      n = 2;
    } else {
      n = 1;
    }
  }

  void set(uint32_t a, uint32_t b) {
    if (!a) {
      n = 0;
    } else {
      if (!b) {
        n = 1;
      } else {
        n = 1 + b;
      }
    }
  }

  void set_exp() { n = UINT32_MAX; }

  bool is_exp() const { return n == UINT32_MAX; }

  std::ostream &put(std::ostream &s) const {
    if (is_exp()) {
      s << "O(2^n)";
      return s;
    }
    if (n > 2) {
      s << "O(n^" << n-1 << ')';
    } else {
      if (n > 1) {
        s << "O(n)";
      } else {
        if (n > 0) {
          s << "O(1)";
        } else {
          s << "O(0)";
        }
      }
    }
    return s;
  }

  Poly & operator=(const Runtime::Poly &p);
  bool operator==(const Runtime::Poly &p) const;

  bool operator==(const Poly &p) const {
    return n == p.n;
  }

  bool operator!=(const Poly &p) const {
    return !(*this == p);
  }

  bool operator==(int i) const {
    assert(i >=0);
    if (!i)
      return n == 0;
    return n == 1;
  }

  bool operator!=(int i) const {
    return !(*this == i);
  }

  bool operator>(const Poly &p) const {
    return n > p.n;
  }

  bool operator>(int a) const {
    if (!a && !n) {
      return false;
    }
    if (n > 1) {
      return true;
    }
    return false;
  }
};

inline std::ostream &operator<<(std::ostream &s, const Poly &p) {
  return p.put(s);
}
}  // namespace Asm


class Poly;
inline Poly operator*(const Poly &p, const Poly &q);


class Poly {
 private:
  std::vector<uint32_t> coefficients;
  uint32_t n;
  bool exponential;

  void init(const Yield::Poly &p) {
    if (p == Yield::UP) {
      coefficients.resize(2);
      coefficients[1] = 1;
      n = 1;
    } else {
      coefficients.resize(1);
      coefficients[0] = p.konst();
      n = 0;
    }
  }

 public:
  Poly() : n(0), exponential(false) {
    coefficients.resize(1);
  }

  explicit Poly(uint32_t a) : n(0), exponential(false) {
    coefficients.resize(1);
    coefficients[0] = a;
  }


  Poly(uint32_t a, uint32_t term) : n(term), exponential(false) {
    assert(a != 0);
    coefficients.resize(term+1);
    coefficients[term] = a;
  }


  explicit Poly(const Yield::Poly &p) : exponential(false) {
    init(p);
  }


  explicit Poly(const Yield::Size &s);
  explicit Poly(const Yield::Multi &s);


  uint32_t degree() const {
    assert(n == 0 || coefficients[n]);
    assert(coefficients.size() == n+1);
    if (exponential) {
      return static_cast<uint32_t>(-1);
    }
    return n;
  }


  void divide_by_n();


  void zero() {
    coefficients.resize(1);
    coefficients[0] = 0;
    exponential = false;
    n = 0;
  }


  bool is_exp() const {
    return exponential;
  }


  bool operator<(const Poly &p) const {
    if (exponential && p.exponential)
      return false;
    if (!exponential && p.exponential)
      return true;
    if (exponential && !p.exponential)
      return false;
    if (n < p.n)
      return true;
    if (n > p.n)
      return false;
    std::vector<uint32_t>::const_reverse_iterator j = p.coefficients.rbegin();
    for (std::vector<uint32_t>::const_reverse_iterator i=coefficients.rbegin();
         i != coefficients.rend() && j != p.coefficients.rend(); ++i, ++j) {
      if (*i == *j) {
        continue;
      }
      if (*i < *j) {
        return true;
      } else {
        if (*i > *j) {
          return false;
        }
      }
    }
    return false;
  }


  bool operator>(const Poly &p) const {
    if (exponential && p.exponential)
      return false;
    if (exponential && !p.exponential)
      return true;
    if (!exponential && p.exponential)
      return false;
    if (n > p.n)
      return true;
    if (n < p.n)
      return false;
    std::vector<uint32_t>::const_reverse_iterator j = p.coefficients.rbegin();
    for (std::vector<uint32_t>::const_reverse_iterator i=coefficients.rbegin();
         i != coefficients.rend() && j != p.coefficients.rend(); ++i, ++j) {
      if (*i == *j) {
        continue;
      }
      if (*i < *j) {
        return false;
      } else {
        if (*i > *j) {
          return true;
        }
      }
    }
    return false;
  }


  bool operator>(uint32_t i) const {
    if (n || exponential)
    return true;
    return coefficients[0] > i;
  }


  Poly &operator|=(const Poly &t) {
    if (*this < t) {
      *this = t;
    }
    return *this;
  }


  friend Poly operator*(const Poly &p, const Poly &q);


  Poly &operator*=(const Poly &p) {
    if (p.exponential)
      exponential = true;
    if (exponential)
      return *this;
    // Poly x = (*this) * p;
    // *this = x;
    Poly y = (*this);
    Poly z = p * y;
    coefficients = z.coefficients;
    n = z.n;
    exponential = z.exponential;
    return *this;
  }


  Poly &operator*=(const Asm::Poly &p) {
    if (p.n == 0) {
      this->zero();
      return *this;
    }
    if (p.n == 1)
      return *this;
    if (p.is_exp()) {
      exponential = true;
      return *this;
    }
    coefficients.insert(coefficients.begin(), p.n - 1, 0);
    n += p.n - 1;
    return *this;
  }


  Poly &operator=(uint32_t a) {
    coefficients.resize(1);
    coefficients[0] = a;
    n = 0;
    exponential = false;
    return *this;
  }


  Poly &operator+=(const Poly &p) {
    if (p.exponential)
      exponential = true;
    if (exponential)
      return *this;

    if (p.coefficients.size() > coefficients.size()) {
      coefficients.resize(p.coefficients.size());
      n = p.n;
    }
    std::vector<uint32_t>::const_iterator j = p.coefficients.begin();
    for (std::vector<uint32_t>::iterator i = coefficients.begin();
         i != coefficients.end() && j != p.coefficients.end(); ++i, j++) {
      (*i) += (*j);
      if (*i < *j)
      *i = UINT32_MAX;
    }
    return *this;
  }


  bool operator==(const Asm::Poly &p) const {
    return p == *this;
  }


  bool operator==(uint32_t a) const {
    return !exponential && n == 0 && coefficients[0] == a;
  }


  bool operator!=(uint32_t a) const {
    return !(*this == a);
  }


  uint32_t & operator[](uint32_t a) {
    assert(a < coefficients.size());
    return coefficients[a];
  }


  const uint32_t & operator[](uint32_t a) const {
    assert(a < coefficients.size());
    return coefficients[a];
  }


  void set(const Poly &p) {
    coefficients = p.coefficients;
    exponential = p.exponential;
    n = p.n;
  }


  void set(uint32_t a, uint32_t b) {
    assert(a != 0);
    // if (b + 1 > coefficients.size())
    coefficients.resize(b+1);
    coefficients[b] = a;
    n = b;
    exponential = false;
  }


  explicit Poly(const Table &t);
  explicit Poly(const std::vector<Table> &t);


  std::ostream &put(std::ostream &s) const;
};


inline std::ostream &operator<<(std::ostream &s, const Poly &p) {
  return p.put(s);
}


inline Poly operator*(const Poly &p, const Poly &q) {
  if (p.is_exp())
    return p;
  if (q.is_exp())
    return q;
  if (p == 1)
    return q;
  if (q == 1)
    return p;
  // FIXME optimise stuff like one arg == 1 etc.
  // -> already fixed (see above) ?!?
  Poly x;
  if (p == 0 || q == 0) {
    return x;
  }
  x.coefficients.resize(p.degree() + q.degree() + 1);
  x.n = p.degree() + q.degree();
  uint32_t a = 0;
  for (std::vector<uint32_t>::const_iterator j=p.coefficients.begin();
       j != p.coefficients.end(); ++j, ++a) {
    uint32_t b = 0;
    for (std::vector<uint32_t>::const_iterator i=q.coefficients.begin();
         i != q.coefficients.end(); ++i, ++b) {
      uint32_t t = 0;
      bool ret = mult_uint32_t(*i, *j, t);
      if (!ret) {
        x.coefficients[a+b] = UINT32_MAX;
      } else {
        x.coefficients[a+b] += t;
        if (x.coefficients[a+b] < t)
        x.coefficients[a+b] = UINT32_MAX;
      }
    }
  }
  assert(x.n == 0 || x.coefficients[x.n] != 0);
  return x;
}


}  // namespace Runtime

#endif  // SRC_RUNTIME_HH_
