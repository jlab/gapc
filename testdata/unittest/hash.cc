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

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE hash
#include <iostream>
#include <vector>
#include <algorithm>
#include <utility>

#include <boost/test/unit_test.hpp>

#include "macros.hh"

#define STATS
#include "../../rtlib/hash.hh"


/* FIXME delete - now in vector_sparse
BOOST_AUTO_TEST_CASE(stack)
{
  Stack<size_t> stack;
  stack.init(1024);
  for (size_t i = 0; i<42; ++i)
    stack.push(i);
}
*/


BOOST_AUTO_TEST_CASE(hash) {
  Hash::Set<size_t> set;
  set.resize(64);
  set.add(size_t(23));
  set.add(size_t(42));
  set.finalize();

  Hash::Set<size_t>::iterator i = set.begin();
  CHECK(i != set.end());
  size_t x = *i;
  if (x == 42u) {
    CHECK_EQ(x, size_t(42));
    ++i;
    CHECK(i != set.end());
    CHECK_EQ(*i, size_t(23));
  } else {
    CHECK_EQ(x, size_t(23));
    ++i;
    CHECK(i != set.end());
    CHECK_EQ(*i, size_t(42));
  }
}

BOOST_AUTO_TEST_CASE(bits) {
  uint32_t t = static_cast<uint32_t>(1) << 31;
  CHECK_EQ(count_leading_zeroes(t), 0);
  uint32_t s = static_cast<uint32_t>(1) << 28;
  CHECK_EQ(count_leading_zeroes(s), 3);

  CHECK_EQ(slow_clz(t), 0);
  CHECK_EQ(slow_clz(s), 3);
}

BOOST_AUTO_TEST_CASE(next_power) {
  uint32_t t = static_cast<uint32_t>(1);
  CHECK_EQ(size_to_next_power(t), static_cast<uint32_t>(1));
  uint32_t u = static_cast<uint32_t>(42);
  CHECK_EQ(size_to_next_power(u), static_cast<uint32_t>(64));
  uint32_t v = static_cast<uint32_t>(128);
  CHECK_EQ(size_to_next_power(v), static_cast<uint32_t>(128));
  uint32_t w = static_cast<uint32_t>(1) << 31;
  CHECK_EQ(size_to_next_power(w), static_cast<uint32_t>(1) << 31);
}

BOOST_AUTO_TEST_CASE(rehash) {
  Hash::Set<size_t> set;
  set.resize(7);
  for (size_t i = 0; i < 129; ++i)
    set.add(i*i);
  set.finalize();
  std::vector<size_t> l;
  for (Hash::Set<size_t>::iterator j = set.begin();
       j != set.end(); ++j)
    l.push_back(*j);
  CHECK_EQ(l.size(), static_cast<uint32_t>(129));
  std::sort(l.begin(), l.end());
  size_t a = 0;
  for (std::vector<size_t>::iterator i = l.begin(); i != l.end(); ++i, ++a)
    CHECK_EQ(*i, a*a);
}


/*
BOOST_AUTO_TEST_CASE(hint)
{
  Hash::Set<size_t> set;
  set.hint(42);
  set.add(size_t(23));
  Hash::Set<size_t>::iterator j = set.begin();
  CHECK(j != set.end());
  CHECK_EQ(*j, size_t(23));
}
*/

#include "../../rtlib/shape.hh"

BOOST_AUTO_TEST_CASE(shape) {
  Hash::Set<Shape> set;
  set.resize(64);
  Shape t;
  t.append('[');
  t.append(']');
  set.add(t);
  set.finalize();
  Hash::Set<Shape>::iterator j = set.begin();
  CHECK(j != set.end());
  CHECK_EQ(*j, t);
}

BOOST_AUTO_TEST_CASE(uint32) {
  uint64_t t = 0;
  t |= static_cast<uint64_t>(1) << 23;
  t |= static_cast<uint64_t>(1) << 42;
  uint32_t lower = t;
  CHECK_EQ(lower, static_cast<uint32_t>(1) << 23);
  uint32_t upper = t >> 32;
  CHECK_EQ(upper, static_cast<uint32_t>(1) << 10);
}


// FIXME
/*
BOOST_AUTO_TEST_CASE(collision)
{
  Hash::Set<Shape> set;
  set.resize(16);
  std::vector<Shape> t;
  t.resize(4);
  t[0].append('[');
  t[0].append(']');

  t[1].append('[');
  t[1].append(']');
  t[1].append('[');
  t[1].append(']');

  t[2].append('[');
  t[2].append('[');
  t[2].append(']');
  t[2].append(']');


  t[3].append('[');
  t[3].append(']');
  t[3].append('[');
  t[3].append('[');
  t[3].append(']');
  t[3].append(']');
  for (std::vector<Shape>::iterator i = t.begin(); i!=t.end(); ++i)
    set.add(*i);
  CHECK_LESS(set.stats.collisions(), 2);
  std::vector<Shape> u;
  for (Hash::Set<Shape>::iterator i = set.begin();
       i!=set.end(); ++i)
    u.push_back(*i);
  CHECK_EQ(u.size(), t.size());
  std::sort(t.begin(), t.end());
  std::sort(u.begin(), u.end());
  std::vector<Shape>::iterator i = t.begin();
  for (std::vector<Shape>::iterator j = u.begin();
      j != u.end() && i != t.end(); ++i, ++j)
    CHECK_EQ(*i, *j);
}
*/



BOOST_AUTO_TEST_CASE(coverage) {
  std::vector<bool> t(64);
  for (std::vector<bool>::iterator i = t.begin(); i != t.end(); ++i)
    CHECK_EQ(*i, false);
  Hash::Multhash hash;
  for (int i = 0; i < 100; ++i) {
    uint32_t x = hash(i, 64);
    CHECK_LESS(x, static_cast<uint32_t>(64));
    t[x] = true;
  }
  for (std::vector<bool>::iterator i = t.begin(); i != t.end(); ++i)
    CHECK_EQ(*i, true);
}


  template <typename T, typename U = uint32_t>
  struct MyInspector {
    T init(const T &x) const { return x; }
    U hash(const T &x) const {
      return x.first;
    }
    void update(T &dst, const T &src) const {
      dst.second += src.second;
    }
    bool equal(const T &a, const T &b) const {
      return a.first == b.first;
    }
    void finalize(T &src) const {
    }

uint32_t k() const { return 0; }
bool cutoff() const { return false; }
bool equal_score(const T &a, const T &b) const {
  CHECK_EQ(0, 1);
  return false;
}
struct compare {
  bool operator()(const T &a, const T &b) const {
    CHECK_EQ(0, 1);
    return false;
  }
};
  };

BOOST_AUTO_TEST_CASE(values) {
  typedef std::pair<int , int> tupel;
  Hash::Set<tupel, MyInspector<tupel> > set;
  set.add(std::make_pair(23, 3));
  set.add(std::make_pair(23, 7));
  set.add(std::make_pair(42, 2));
  set.finalize();
  int array[100] = { 0 };
  for (Hash::Set<tupel, MyInspector<tupel> >::iterator i =
         set.begin();
       i != set.end(); ++i) {
    tupel t = *i;
    array[t.first] = t.second;
  }
  CHECK_EQ(array[23], 10);
  CHECK_EQ(array[42], 2);
}

BOOST_AUTO_TEST_CASE(ref) {
  typedef std::pair<int , int> tupel;
  typedef Hash::Ref<tupel, MyInspector<tupel> > ref;
  ref href;
  href->add(std::make_pair(42, 23));
  ref a = href;
  ref b = href;
  // CHECK_EQ(b.ref().ref_count, unsigned(3));
}


  template <typename T, typename U = uint32_t>
  struct PfInspector {
    double sum;
    PfInspector() : sum(0) {}
    T init(const T &x) const { return x; }
    U hash(const T &x) const {
      return hashable_value(x.first);
    }
    void update(T &dst, const T &src) {
      if (src.second.first < dst.second.first)
        dst.second.first = src.second.first;
      dst.second.second += src.second.second;
      sum += src.second.second;
    }
    bool equal(const T &a, const T &b) const {
      return a.first == b.first;
    }
    bool filter() const { return true; }
    bool filter(const T &x) const {
      double thresh = 0.000001 * sum;
      return x.second.second <= thresh;
    }
    void finalize(T &src) const {
    }

uint32_t k() const { return 0; }
bool cutoff() const { return false; }
bool equal_score(const T &a, const T &b) const {
  CHECK_EQ(0, 1);
  return false;
}
struct compare {
  bool operator()(const T &a, const T &b) const {
    CHECK_EQ(0, 1);
    return false;
  }
};
  };

BOOST_AUTO_TEST_CASE(filter) {
  typedef std::pair<Shape, std::pair<double, double> > tupel;
  typedef Hash::Ref<tupel, PfInspector<tupel> > ref;
  ref h;
  tupel t;
  append(t.first, "[]", 2);
  t.second.first = 7;
  t.second.second = 0;
  push_back(h, t);
  append(t.first, "[]", 2);
  t.second.first = 6;
  t.second.second = 1;
  push_back(h, t);
  t.second.first = 5;
  t.second.second = 2;
  push_back(h, t);
  h->filter();
  h->finalize();
  for (ref::iterator i = h->begin(); i != h->end(); ++i) {
    CHECK_EQ((*i).second.first, 5);
    CHECK_EQ((*i).second.second, 3);
  }
  // h->purge();
}


/*
BOOST_AUTO_TEST_CASE(erase_test)
{
  typedef std::pair<Shape, std::pair<double, double> > tupel;
  typedef Hash::Ref<tupel, PfInspector<tupel> > ref;
  ref h;
  tupel t;
  append(t.first, "[]", 2);
  t.second.first = 7;
  t.second.second = 2;
  push_back(h, t);

  tupel u;
  t = u;
  append(t.first, "[]", 2);
  append(t.first, "[]", 2);
  t.second.first = 6;
  t.second.second = 1;
  push_back(h, t);

  tupel v;
  t = v;
  append(t.first, "[[", 2);
  append(t.first, "]]", 2);
  t.second.first = 5;
  t.second.second = 2;
  push_back(h, t);

  CHECK_EQ(h->used(), unsigned(3));
  ref::iterator i = h->begin();
  ++i;
  i.erase();
  CHECK_EQ(h->used(), unsigned(2));
  ++i;
  CHECK(i != h->end());
  ++i;
  CHECK(i == h->end());
  h->purge();
}
*/

#include "../../rtlib/push_back.hh"

class insp_hash_h {
 public:
  typedef std::pair<Shape, int>  type;

 public:
  uint32_t hash(const type &x) const {
    return hashable_value(left_most(x));
  }
  type init(const type &src) const {
    type dst(src);
    {
      return src;
    }
  }
  void update(type &dst, const type &src) {
    if ((src.second < dst.second)) {
      dst.second = src.second;
    }
  }
  bool equal(const type &a, const type &b) const {
  return left_most(a) == left_most(b);
  }
  bool filter() const { return false; }
  bool filter(const type &x) const {
  assert(0); return false;
  }
  void finalize(const type &src) const {
  }

  uint32_t k() const { return 2; }
  bool cutoff() const { return true; }
  bool equal_score(const type &a, const type &b) const {
    return a.second == b.second;
  }
  struct compare {
    bool operator()(const type &a, const type &b) const {
      return a.second < b.second;
    }
  };
};

BOOST_AUTO_TEST_CASE(cutoff) {
  typedef std::pair<Shape, int> tupel;
  typedef Hash::Ref<tupel, insp_hash_h > ref;
  ref h;
  tupel t;
  append(t.first, "[]", 2);
  t.second = 3;
  push_back(h, t);
  append(t.first, "[]", 2);
  t.second = 3;
  push_back(h, t);
  append(t.first, "[]", 2);
  t.second = 2;
  push_back(h, t);
  append(t.first, "[]", 2);
  t.second = 2;
  push_back(h, t);
  h->filter();
  h->finalize();
  size_t a = 0;
  const int d[4] = { 2, 2, 3, 3 };
  for (ref::iterator i = h->begin(); i != h->end(); ++i, ++a)
    if (a < 4)
      CHECK_EQ((*i).second, d[a]);
    else
      CHECK_EQ(23, 46);
}

template <typename T, typename Size, typename alphset >
inline void append(Fiber<T, Size, alphset> *s, const char *c) {
  for (size_t i = 0; i < std::strlen(c); ++i)
    s->append(c[i]);
}

BOOST_AUTO_TEST_CASE(cutoff_big) {
  typedef std::pair<Shape, int> tupel;
  typedef Hash::Ref<tupel, insp_hash_h > ref;
  ref h;
  tupel t;

  t = tupel();
  append(&t.first, "[][]");
  t.second = -930;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][]][]");
  t.second = -920;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[][][]");
  t.second = -730;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[][[][]]");
  t.second = -680;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[][[][]][]");
  t.second = -480;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[][][][]");
  t.second = 541;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][]][][]");
  t.second = -40;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][]][[][]]");
  t.second = 130;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][]][][][]");
  t.second = 980;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[][[][][]]");
  t.second = 150;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][]][[][][]]");
  t.second = 720;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][][]][]");
  t.second = -250;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][][]][][]");
  t.second = 290;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][][]][[][]]");
  t.second = 610;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][[][]]][]");
  t.second = 410;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][[][]]][][]");
  t.second = 890;
  push_back(h, t);
  t = tupel();
  append(&t.first, "[[][[][]]][[][]]");
  t.second = 1350;
  push_back(h, t);

  h->filter();
  h->finalize();
  std::vector<tupel> v;
  for (ref::iterator i = h->begin(); i != h->end(); ++i) {
    v.push_back(*i);
  }
  CHECK_EQ(v.size(), 2);
  if (v.size() == 2) {
    CHECK_EQ(v.front().second, -930);
    CHECK_EQ(v.back().second, -920);
  }
}

