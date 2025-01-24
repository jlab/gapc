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


#ifndef SRC_YIELDSIZE_HH_
#define SRC_YIELDSIZE_HH_

#include <iostream>
#include <cassert>
#include <vector>
#include <list>

// tr1 has it
#include <boost/cstdint.hpp>

class Filter;


namespace Yield {


enum up { UP };


class Poly {
 private:
    uint32_t i;
    bool n;

 public:
    Poly() : i(0), n(false) {}
    explicit Poly(uint32_t a) : i(a), n(false) {}
    explicit Poly(up a) : i(0), n(true) {}

    void set(uint32_t a) { i = a; n = false; }
    void set(up a) { n = true; }

    uint32_t konst() const { return i; }

    Poly &operator+=(const Poly &p) {
      n = p.n || n;
      if (!n) {
        i += p.i;
      }
      return *this;
    }

    Poly &operator/=(const Poly &p) {
      if (n && !p.n) {
        n = false;
        i = p.i;
      } else if (!n && !p.n) {
        i = i > p.i ? p.i : i;
      }
      return *this;
    }

    Poly &operator*=(const Poly &p) {
      if (p.n) {
        n = p.n;
      } else {
        i = p.i > i ? p.i : i;
      }
      return *this;
    }

    bool operator==(const Poly &p) const {
      if (n && p.n) {
        return true;
      }
      if (!n && !p.n) {
        return i == p.i;
      }
      return false;
    }

    bool operator==(int a) const {
      assert(a >= 0);
      return !n && i == static_cast<uint32_t>(a);
    }

    bool operator!=(const Poly &p) const {
      return !(*this == p);
    }

    bool operator>(int a) const {
      assert(a >= 0);
      uint32_t b = static_cast<uint32_t>(a);
      if (n) {
        return true;
      }
      return i > b;
    }

    bool operator<(const Poly &p) const {
      if (n && p.n) {
        return false;
      }
      if (!n && p.n) {
        return true;
      }
      if (n && !p.n) {
        return false;
      }
      return i < p.i;
    }

    bool operator>(const Poly &p) const {
      if (*this == p) {
        return false;
      }
      return !((*this) < p);
    }

    bool operator<(up u) const {
      return !n;
    }

    bool operator==(up u) const {
      return n;
    }

    Poly &operator=(uint32_t a) {
      i = a;
      n = false;
      return *this;
    }

    Poly &operator=(up u) {
      i = 0;
      n = true;
      return *this;
    }

    Poly &operator-=(const Poly &p)  {
      if (n || p.n) {
        return *this;
      }
      if (i < p.i) {
        i = 0;
      } else {
        i -= p.i;
      }
      return *this;
    }

    std::ostream &put(std::ostream &s) const {
      if (n) {
        s << 'n';
      } else {
        s << i;
      }
      return s;
    }
};


inline std::ostream &operator<<(std::ostream &s, const Poly &p) {
  return p.put(s);
}


class Size {
 private:
    Poly min;
    Poly max;
    bool fresh;


 public:
    Size() : fresh(false) {
    }


    explicit Size(up foo) : fresh(true) {
      min.set(0); max.set(UP);
    }


    Size(uint32_t a, up b) : fresh(true) {
      min.set(a); max.set(b);
    }


    Size(up a, uint32_t b) : fresh(true) {
      min.set(a); max.set(b);
    }


    Size(const Poly &a, const Poly &b) : fresh(true) {
      min = a; max = b;
    }


    bool initialized() {
      return fresh;
    }


    // FIXME not needed - see Poly()
    void set(uint32_t a, up b) {
      fresh = true; min.set(a); max.set(b);
    }


    void set(up a, uint32_t b) {
      fresh = true; min.set(a); max.set(b);
    }


    void set(up a, up b) {
      fresh = true; min.set(a); max.set(b);
    }


    void set(uint32_t a, uint32_t b) {
      fresh = true; min.set(a); max.set(b);
    }


    void set(const Poly &a, const Poly &b) {
      fresh = true; min = a; max = b;
    }


    void set_high(const Poly &b) {
      fresh = true; max = b;
    }


    const Poly & low() const {
      return min;
    }


    const Poly & high() const {
      return max;
    }


    Poly & high() {
      return max;
    }


    Size &operator+=(const Size &s) {
      fresh = true;
      min += s.min;
      max += s.max;
      return *this;
    }


    Size &operator/=(const Size &s) {
      if (!fresh) {
        fresh = true;
        min = s.min;
        max = s.max;
      }
      min /= s.min;
      max *= s.max;
      return *this;
    }


    bool operator==(const Size &s) const {
      return min == s.min && max == s.max && fresh == s.fresh;
    }


    bool operator!=(const Size &s) const {
      return !(*this == s);
    }


    std::ostream &put(std::ostream &s) const {
      if (fresh) {
        s << '(' << min << ", " << max << ')';
      } else {
        s << "(undef)";
      }
      return s;
    }


 private:
    void with_min(const Filter &f);
    void with_max(const Filter &f);


 public:
    void with(const std::list<Filter*> &l);
};


inline std::ostream &operator<<(std::ostream &s, const Size &p) {
  return p.put(s);
}


extern const Size Initial;


class Multi {
 private:
    std::vector<Size> array;

 public:
    Multi() {
    }


    void set_tracks(size_t l) {
      array.resize(l);
    }


    size_t tracks() const { return array.size(); }


    explicit Multi(size_t l) {
      set_tracks(l);
    }


    Size &operator()(size_t i) {
      assert(i < array.size());
      return array[i];
    }


    const Size &operator()(size_t i) const {
      assert(i < array.size());
      return array[i];
    }


    Multi &operator+=(const Multi &o) {
      [[maybe_unused]] size_t a = array.size(), b = o.array.size();
      assert(a);
      assert(a == b);
      std::vector<Size>::const_iterator j = o.array.begin();
      for (std::vector<Size>::iterator i = array.begin();
      i != array.end(); ++i, ++j) {
        *i += *j;
      }
      return *this;
    }


    Multi &operator/=(const Multi &o) {
      assert(array.size());
      assert(array.size() == o.array.size());
      std::vector<Size>::const_iterator j = o.array.begin();
      for (std::vector<Size>::iterator i = array.begin();
      i != array.end(); ++i, ++j) {
        *i /= *j;
      }
      return *this;
    }


    bool operator==(const Multi &o) const {
      assert(array.size());
      assert(array.size() == o.array.size());
      std::vector<Size>::const_iterator j = o.array.begin();
      for (std::vector<Size>::const_iterator i = array.begin();
      i != array.end(); ++i, ++j) {
        if (*i != *j) {
          return false;
        }
      }
      return true;
    }


    bool operator!=(const Multi &o) const {
      return !(*this == o);
    }


    typedef std::vector<Size>::iterator iterator;


    iterator begin() {
      return array.begin();
    }


    iterator end() {
      return array.end();
    }


    typedef std::vector<Size>::const_iterator const_iterator;


    const_iterator begin() const {
      return array.begin();
    }


    const_iterator end() const {
      return array.end();
    }


    void put(std::ostream &s) const;


    void with(const std::vector<std::list<Filter*> > &l);

    bool is_low_zero() const;

    void min_high(const Multi &o);
    void max_high(const Multi &o);
    void sub_high_low(const Multi &o);
    bool leq_high(const Multi &o) const;

    bool has_moving() const;
};


inline std::ostream &operator<<(std::ostream &s, const Multi &p) {
  p.put(s);
  return s;
}


}  // namespace Yield


#endif  // SRC_YIELDSIZE_HH_
