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
#define BOOST_TEST_MODULE rope
#include <iostream>
#include <ctime>
#include <cstring>
#include <string>
#include <boost/test/unit_test.hpp>

#include "macros.hh"

#include "../../rtlib/subsequence.hh"
#include "../../rtlib/rope.hh"
#define STATS
#include "../../rtlib/hash.hh"


BOOST_AUTO_TEST_CASE(rope_init) {
  Rope s;
  s.append('y');
  Rope r;
  r.append('x');
  // std::cerr << r << std::endl;
  r.append(s);
  // std::cerr << r << std::endl;
  r.append("hello", 5);
  // std::cerr << r << std::endl;
}

BOOST_AUTO_TEST_CASE(rope_rep) {
  Rope s;
  s.append('.', 5);
  Rope t;
  t.append(".....", 5);
  CHECK_EQ(s, t);
  Rope u;
  append(u, '.', 5);
  CHECK_EQ(s, u);
}

BOOST_AUTO_TEST_CASE(rt_rope) {
  Rope a;
  a.append("123456", 6);
  Rope b;
  b.append("113456", 6);
  CHECK_NOT_EQ(a, b);
  Rope c;
  c.append("123", 3);
  c.append("456", 3);
  CHECK_EQ(a, c);
  Rope s;
  s.append('x');
  Rope t;
  t.append('+');
  s.append(t);
  s.append("xyz", 3);
  std::ostringstream o;
  o << s;
  CHECK_EQ(o.str(), "x+xyz");
}

BOOST_AUTO_TEST_CASE(rope_len) {
  Rope t;
  t.append("foobar", 6);
  Rope r;
  r.append("((", 2);
  r.append('.', 3);
  r.append(t);
  r.append('.', 4);
  r.append("))", 2);
}

BOOST_AUTO_TEST_CASE(rope_eq) {
  Rope a;
  Rope b;
  Rope c;
  Rope d;
  Rope e;
  Rope f;

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

static void app(Rope *a, const std::string &s) {
  a->append(s.c_str(), s.size());
}

#include <boost/random/linear_congruential.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/random.hpp>

typedef boost::mt19937 rand_gen;
typedef boost::uniform_int<> rand_dist;

BOOST_AUTO_TEST_CASE(rope_rand) {
  rand_gen gen(static_cast<unsigned int>(std::time(0)));
  boost::variate_generator<rand_gen&, rand_dist>
    die_alph(gen, rand_dist(0, 2));
  boost::variate_generator<rand_gen&, rand_dist>
    die_len(gen, rand_dist(1, 100));

  const char alph[3] =
     { '[', ']', '_' };

  for (int i = 0; i < 100; ++i) {
    Rope a, b, x;
    std::string s, t;
    for (int a = 0; a < die_len(); ++a) {
      s.push_back(alph[die_alph()]);
    }
    for (int a = 0; a < die_len(); ++a) {
      t.push_back(alph[die_alph()]);
    }
    app(&a, s);
    app(&b, t);
    // std::cerr << "App: " << s << " " << t << '\n';
    x.append(a);
    x.append(b);
    std::ostringstream o;
    o << x;
    CHECK_EQ(o.str(), s+t);
  }
}

BOOST_AUTO_TEST_CASE(cstring) {
  Rope s = Rope("Hello");
  append(s, " World");
  CHECK_EQ(s, Rope("Hello World"));
}

#include "../../rtlib/list.hh"

BOOST_AUTO_TEST_CASE(uniques) {
  List_Ref<Rope> l;
  List_Ref<Rope> r = unique(l);
}

/* FIXME rework stats for hashtng

BOOST_AUTO_TEST_CASE(collision)
{
  //Rope s;
  //std::cerr << hashable_value(s);

  Hash::Set<Rope> set;
  //set.init(16);
  set.resize(16);

  std::vector<Rope> t;
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
  for (std::vector<Rope>::iterator i = t.begin(); i!=t.end(); ++i)
    set.add(*i);
  CHECK_LESS(set.stats.collisions(), 2);
  std::vector<Rope> u;
  for (Hash::Set<Rope>::discard_iterator i = set.discard_begin();
       i!=set.discard_end(); ++i)
    u.push_back(*i);
  CHECK_EQ(u.size(), t.size());
  std::sort(t.begin(), t.end());
  std::sort(u.begin(), u.end());
  std::vector<Rope>::iterator i = t.begin();
  for (std::vector<Rope>::iterator j = u.begin();
      j != u.end() && i != t.end(); ++i, ++j)
    CHECK_EQ(*i, *j);
}

*/

BOOST_AUTO_TEST_CASE(front_) {
  Rope s = Rope("Hello");
  CHECK_EQ('H', front(s));
}

BOOST_AUTO_TEST_CASE(empty_rope) {
  Rope s("");
  Rope x = s;
}

BOOST_AUTO_TEST_CASE(is_empty_rope) {
  Rope s("");
  CHECK_EQ(Rope(""), s);
}


BOOST_AUTO_TEST_CASE(empty_ass) {
  Rope s, t;
  t.append('x', 0);
  s = t;
  CHECK_EQ(s, Rope(""));
}

BOOST_AUTO_TEST_CASE(empty_copy) {
  Rope s;
  Rope x;
  Rope y;
  s.append(x);
  y = s;
  CHECK_EQ(y, Rope(""));
}

BOOST_AUTO_TEST_CASE(empty_copy2) {
  Rope s;
  Rope x;
  x.append("sh", 0);
  Rope y;
  s.append(x);
  y = s;
  CHECK_EQ(y, Rope(""));
}

BOOST_AUTO_TEST_CASE(rope_size) {
  Rope s;
  s.append("foo bar");
  s.append("fuz baz");
  CHECK_EQ(s.size(), 14u);
}

BOOST_AUTO_TEST_CASE(rope_cp_app) {
  Rope r;
  r.append("((", 2);
  Rope s;
  s = r;
  s.append('(');
}

BOOST_AUTO_TEST_CASE(app_subseq) {
  Rope r;
  Sequence s;
  const char inp[] = "foobazbar";
  s.copy(inp, std::strlen(inp));
  Subsequence x(s, 3, 6);
  append_deep(r, x);
  Rope q;
  append(q, "baz");
  CHECK_EQ(r, q);
}

#include "../../rtlib/terminal.hh"

BOOST_AUTO_TEST_CASE(ROPE_TEST) {
  Rope r;
  Sequence s;
  const char inp[] = "foobazbar";
  s.copy(inp, std::strlen(inp));
  r = ROPE(s, 3, 6);
  Rope q;
  append(q, "baz");
  CHECK_EQ(r, q);
}


BOOST_AUTO_TEST_CASE(rope_iter) {
  Rope r;
  append(r, "foobar");
  std::string s;
  for (Rope::iterator i = r.begin(); i != r.end(); ++i)
    s.push_back(*i);
  std::ostringstream o;
  o << r;
  CHECK_EQ(s, o.str());
}

BOOST_AUTO_TEST_CASE(const_rope_iter) {
  Rope r;

  Rope::iterator a = r.begin();
  [[maybe_unused]] Rope::const_iterator b = a;

  append(r, "foobar");
  std::string s;
  for (Rope::const_iterator i = r.begin(); i != r.end(); ++i)
    s.push_back(*i);
  std::ostringstream o;
  o << r;
  CHECK_EQ(s, o.str());
}

BOOST_AUTO_TEST_CASE(long_rope_iter) {
  Rope r;
  append(r,
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoo"
      "barfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoo"
      "barfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar"
      "foobarfoobarfoobarfoobarfoobarfoobarfoobarfoobar");
  std::string s;
  for (Rope::iterator i = r.begin(); i != r.end(); ++i)
    s.push_back(*i);
  std::ostringstream o;
  o << r;
  CHECK_EQ(s, o.str());
}
