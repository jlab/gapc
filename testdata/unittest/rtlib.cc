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

// link against -lboost_unit_test_framework if boost/test/unit_test.hpp is
// used ...

// see also https://bugs.launchpad.net/ubuntu/+source/boost/+bug/162155
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE rtlib
#include <algorithm>
#include <iostream>
#include <utility>

#include <boost/test/unit_test.hpp>

// include everything - no linking needed ...
// #define BOOST_TEST_MAIN
// #include <boost/test/included/unit_test_framework.hpp>

#include "macros.hh"

#include "../../rtlib/list.hh"
#include "../../rtlib/algebra.hh"
#include "../../rtlib/table.hh"
#include "../../rtlib/terminal.hh"
#include "../../rtlib/filter.hh"
#include "../../rtlib/string.hh"
#include "../../rtlib/push_back.hh"


BOOST_AUTO_TEST_CASE(listtest) {
  List<int> l;
  for (int i = 0; i < 100; i++)
    l.push_back(i);
  int x = 0;
  for (List<int>::iterator i = l.begin(); i != l.end(); ++i) {
    CHECK_EQ(*i, x);
    x++;
  }
  CHECK_EQ(x, 100);
  CHECK_NOT_EQ(l.empty(), true);
  l.clear();
  x = 0;
  for (List<int>::iterator i = l.begin(); i != l.end(); ++i)
    x++;
  CHECK_EQ(x, 0);
  CHECK_EQ(l.empty(), true);

  int g;
  empty(g);
  CHECK_NOT_EQ(is_not_empty(g), true);
}

BOOST_AUTO_TEST_CASE(algebra) {
  List<int> l;
  for (int i = 100; i > 0; i--) {
    int a = i + i % 2;
    l.push_back(a);
  }
  typedef typename List<int>::iterator itr;
  std::pair<itr, itr> p = std::make_pair(l.begin(), l.end());
  l = unique(p).ref();

  size_t size = 0;
  for (List<int>::iterator i = l.begin(); i != l.end(); ++i)
    size++;
  CHECK_EQ(size, size_t(50));

  int i = minimum(l.begin(), l.end());
  CHECK_EQ(i, 2);

  i = maximum(l.begin(), l.end());
  CHECK_EQ(i, 100);

  i = sum(l.begin(), l.end());
  CHECK_EQ(i, 2550);
}

BOOST_AUTO_TEST_CASE(table) {
  Table::Constant<int> a(10);
  int x = 23;
  for (int y = 0; y < 5; y++) {
    tabulate(a, 1, y, x);
    x += y + 1;
  }
  x = 23;
  for (int y = 0; y < 5; y++) {
    CHECK(is_tabulated(a, 1, y));
    CHECK_EQ(x, get_tabulated(a, 1, y));
    x += y + 1;
  }
  x = 42;
  Table::Linear<Table::Left, int> b(10);
  for (int y = 0; y < 5; y++)
    for (int z = 0; z < 5; z++) {
      tabulate(b, y, z, x);
      x += y + 1;
    }
  x = 42;
  for (int y = 0; y < 5; y++)
    for (int z = 0; z < 5; z++) {
      CHECK(is_tabulated(b, y, z));
      CHECK_EQ(x, get_tabulated(b, y, z));
      x += y + 1;
    }
  x = 66;
  Table::Quadratic<int, Table::Unger, unsigned int,
                   Table::RawIndex<unsigned int> > t(10);
  for (int y = 0; y < 10; y++)
    for (int z = 0; z < 10; z++) {
      tabulate(t, y, z, x);
      x += y + 1;
    }
  x = 66;
  for (int y = 0; y < 10; y++)
    for (int z = 0; z < 10; z++) {
      CHECK(is_tabulated(t, y, z));
      CHECK_EQ(x, get_tabulated(t, y, z));
      x += y + 1;
    }

  x = 66;
  Table::Quadratic<int> u(10);
  for (int y = 0; y < 10; y++)
    for (int z = y; z < 10; z++) {
      tabulate(u, y, z, x);
      x += y + 1;
    }
  x = 66;
  for (int y = 0; y < 10; y++)
    for (int z = y; z < 10; z++) {
      CHECK(is_tabulated(u, y, z));
      CHECK_EQ(x, get_tabulated(u, y, z));
      x += y + 1;
    }
}

BOOST_AUTO_TEST_CASE(term) {
  static char s[] = "123Hello world!";
  Sequence seq(s);
  int i = INT(seq, 0, 3);
  CHECK_EQ(i, 123);
  char a = CHAR(seq, 3, 4);
  CHECK_EQ(a, 'H');
  char b = CHAR(seq, 4, 5, 'e');
  CHECK_EQ(b, 'e');
  char c = CHAR(seq, 4, 5, 'H');
  CHECK(isEmpty(c));
  int j = SEQ(seq, 1, 3);
  CHECK_EQ(j, 2);
}

BOOST_AUTO_TEST_CASE(leer) {
  int i = 0;
  CHECK(!isEmpty(i));
  empty(i);
  CHECK(isEmpty(i));
  double d = 0;;
  CHECK(!isEmpty(d));
  empty(d);
  CHECK(isEmpty(d));
  bool b = true;
  CHECK(!isEmpty(b));
  empty(d);
  char c = 'c';
  CHECK(!isEmpty(c));
  empty(c);
  CHECK(isEmpty(c));
  c = 'c';
  String s;
  CHECK(!isEmpty(s));
  s.append(c);
  CHECK(!isEmpty(s));
  empty(s);
  CHECK(isEmpty(s));
  s.append(c);
  CHECK(!isEmpty(s));
  List_Ref<String> l;
  CHECK(isEmpty(l));
  push_back(l, s);
  CHECK(!isEmpty(l));
  empty(l);
  CHECK(!isEmpty(l));
  l.ref().clear();
  CHECK(isEmpty(l));
}

BOOST_AUTO_TEST_CASE(filter) {
  static char s[] = "acgtuACGTUacgtu";
  Sequence seq(s);
  CHECK(!char_basepairing(seq, 0, 1));
  CHECK(char_basepairing(seq, 0, 4));
  CHECK(char_basepairing(seq, 0, 5));
  CHECK(!char_basepairing(seq, 3, 1));
  CHECK(char_basepairing(seq, 1, 3));
  CHECK(char_basepairing(seq, 1, 3));
  CHECK(char_basepairing(seq, 1, 8));
  CHECK(char_basepairing(seq, 6, 8));
  CHECK(char_basepairing(seq, 5, 14));
  CHECK(char_basepairing(seq, 7, 15));
  CHECK(char_basepairing(seq, 9, 13));
  CHECK(char_basepairing(seq, 9, 11));
}

BOOST_AUTO_TEST_CASE(list_ref) {
  List_Ref<int> l;
  List_Ref<int> r;
  r = l;
  CHECK(isEmpty(l));
  int i = 23;
  push_back(r, i);
  // old pointer like semantic:
  // CHECK(!isEmpty(l));
  CHECK(isEmpty(l));
}

BOOST_AUTO_TEST_CASE(filter_equal) {
  Sequence a;
  CHECK(!equal(a, 0, 0));
  static char s[] = "aa";
  a = Sequence(s);
  CHECK(equal(a, 0, 2));
  static char t[] = "ab";
  a = Sequence(t);
  CHECK(!equal(a, 0, 2));
}

BOOST_AUTO_TEST_CASE(pushback) {
  List_Ref<int> l;
  int a = 23;
  int b = 42;
  int c = 41;
  push_back_max(l, a);
  CHECK_EQ(l.ref().size(), size_t(1));
  push_back_max(l, b);
  CHECK_EQ(l.ref().size(), size_t(1));
  push_back_max(l, c);
  CHECK_EQ(l.ref().size(), size_t(1));
  CHECK_EQ(l.ref().front(), 42);

  List_Ref<int> m;
  a = 23;
  b = 42;
  c = 41;
  push_back_min(m, c);
  CHECK_EQ(m.ref().size(), size_t(1));
  push_back_min(m, b);
  CHECK_EQ(m.ref().size(), size_t(1));
  push_back_min(m, a);
  CHECK_EQ(m.ref().size(), size_t(1));
  CHECK_EQ(m.ref().front(), 23);

  List_Ref<int> n;
  a = 23;
  b = 42;
  c = 41;
  push_back_sum(n, a);
  CHECK_EQ(n.ref().size(), size_t(1));
  push_back_sum(n, b);
  CHECK_EQ(n.ref().size(), size_t(1));
  push_back_sum(n, c);
  CHECK_EQ(n.ref().size(), size_t(1));
  CHECK_EQ(n.ref().front(), 106);
}

BOOST_AUTO_TEST_CASE(pushback_other) {
  List_Ref<std::pair<int, String> > l;
  std::pair<int, String> p;
  p.first = 23;
  push_back_max_other(l, p);
  CHECK_EQ(l.ref().size(), size_t(1));
  std::pair<int, String> q;
  q.first = 42;
  push_back_max_other(l, q);
  CHECK_EQ(l.ref().size(), size_t(1));
  std::pair<int, String> r;
  r.first = 41;
  push_back_max_other(l, r);
  CHECK_EQ(l.ref().size(), size_t(1));
  CHECK_EQ(l.ref().front().first, 42);


  List_Ref<std::pair<int, String> > m;
  push_back_min_other(m, p);
  CHECK_EQ(m.ref().size(), size_t(1));
  push_back_min_other(m, q);
  CHECK_EQ(m.ref().size(), size_t(1));
  push_back_min_other(m, r);
  CHECK_EQ(m.ref().size(), size_t(1));
  CHECK_EQ(m.ref().front().first, 23);
}

BOOST_AUTO_TEST_CASE(string_rep) {
  String s;
  s.append('.', 5);
  String t;
  t.append(".....", 5);
  CHECK_EQ(s, t);
  String u;
  append(u, '.', 5);
  CHECK_EQ(s, u);
}

BOOST_AUTO_TEST_CASE(rt_string) {
  String a;
  a.append("123456", 6);
  String b;
  b.append("113456", 6);
  CHECK_NOT_EQ(a, b);
  String c;
  c.append("123", 3);
  c.append("456", 3);
  CHECK_EQ(a, c);
  String s;
  s.append('x');
  String t;
  t.append('+');
  s.append(t);
  s.append("xyz", 3);
  std::ostringstream o;
  o << s;
  CHECK_EQ(o.str(), "x+xyz");
}

BOOST_AUTO_TEST_CASE(string_len) {
  String t;
  t.append("foobar", 6);
  String r;
  r.append("((", 2);
  r.append('.', 3);
  r.append(t);
  r.append('.', 4);
  r.append("))", 2);
}

BOOST_AUTO_TEST_CASE(string_eq) {
  String a;
  String b;
  String c;
  String d;
  String e;
  String f;

  b.append("123", 3);
  c.append("45", 2);
  a.append(b);
  a.append(c);

  e.append("12", 2);
  f.append("345", 3);
  d.append(e);
  d.append(f);

  CHECK_EQ(a, d);
}

BOOST_AUTO_TEST_CASE(string_lt) {
  String a;
  a.append("anna", 4);
  String b;
  b.append("ann", 3);
  CHECK_LESS(b, a);
  String c;
  c.append("anna", 4);
  CHECK(!(a < c));
  CHECK(!(a < b));
  String x;
  x.append("Arlene", 6);
  String y;
  y.append("Arline", 6);
  CHECK_LESS(x, y);
}

BOOST_AUTO_TEST_CASE(min_max_empty) {
  List_Ref<int> l;
  int x = maximum(l.ref().begin(), l.ref().end());
  CHECK(isEmpty(x));
  List_Ref<int> m;
  int y = minimum(m.ref().begin(), m.ref().end());
  CHECK(isEmpty(y));
}

BOOST_AUTO_TEST_CASE(bit_ops) {
  unsigned int x(-1);
  unsigned int y(-1);
  CHECK_NOT_EQ(x, y >> 1);
}

BOOST_AUTO_TEST_CASE(string_neg) {
  String s;
  s.append(10);
  s.append('c');
  s.append(9);
  s.append(-23);
  s.append('c');
  s.append(-9);
  s.append("[]", 2);
  std::stringstream o;
  o << s;
  CHECK_EQ(o.str(), "10c9-23c-9[]");
}


