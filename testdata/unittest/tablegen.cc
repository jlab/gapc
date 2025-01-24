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
#define BOOST_TEST_MODULE tablegen

// include everything - no linking needed ...
// #define BOOST_TEST_MAIN
// #include <boost/test/included/unit_test_framework.hpp>

#include <map>
#include <set>
#include <string>
#include <sstream>
#include <vector>

#include <boost/test/unit_test.hpp>

#include "macros.hh"

#include "../../src/tablegen.hh"
#include "../../src/cpp.hh"
#include "expr_parser.hh"
#include "../../src/lexer.h"
#include "../../src/lexer_priv.h"

BOOST_AUTO_TEST_CASE(parser) {
  std::string s("(1+2)*3+x");
  yy_scan_string(s.c_str());

  std::map<std::string, int> look;
  look["x"] = 4;
  int result = 0;
  yy::Expr_Parser parser(look, result);
  parser.parse();
  CHECK_EQ(result, 13);
}

#include "../../src/expr/base.hh"

typedef std::map<std::string, int> env_t;

static int parse(Expr::Base *e, env_t x) {
  std::ostringstream o;
  o << *e;
  yy_scan_string(o.str().c_str());
  int result = 0;
  yy::Expr_Parser parser(x, result);
  parser.parse();
  return result;
}

typedef std::set<int> set_t;

BOOST_AUTO_TEST_CASE(cons) {
  std::vector<Table> v;
  Table t;
  t |= Table::CONSTANT;
  t.set_left_rest(Yield::Size(Yield::Poly(0), Yield::Poly(2)));
  t.set_right_rest(Yield::Size(Yield::Poly(0), Yield::Poly(3)));
  v.push_back(t);

  Printer::Cpp cpp;

  Tablegen tg;

  tg.offset(0, v.begin(), v.end());

  set_t m;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 4; ++j) {
      env_t e;
      // e["t_0_i"] = i;
      e["t_0_real_i"] = i;
      e["t_0_real_j"] = j;
      int r = parse(tg.off, e);
      set_t::iterator a = m.find(r);
      CHECK(a == m.end());
      m.insert(r);

      if (i == 2 && j == 3)
        CHECK_EQ(r, 11);
      if (i == 0 && j == 0)
        CHECK_EQ(r, 0);
    }

  env_t e;
  e["t_0_n"] = 2;
  e["t_1_n"] = 3;
  int s = parse(tg.size, e);
  CHECK_EQ(s, 12);

  /*
  std::cerr << *tg.size << '\n';
  std::cerr << *tg.off << '\n';
  cpp.print(tg.code);
  */
}

BOOST_AUTO_TEST_CASE(left_lin) {
  std::vector<Table> v;
  Table t;
  t |= Table::LINEAR;
  t.set_sticky(Table::LEFT);
  v.push_back(t);
  v.push_back(t);

  Printer::Cpp cpp;

  Tablegen tg;

  tg.offset(0, v.begin(), v.end());

  set_t m;
  for (int a = 0; a < 13; ++a)
    for (int b = 0; b < 23; ++b) {
      env_t e;
      e["t_0_i"] = 0;
      e["t_1_i"] = 0;
      e["t_0_j"] = a;
      e["t_1_j"] = b;
      e["t_1_n"] = 22;
      int r = parse(tg.off, e);
      set_t::iterator x = m.find(r);
      CHECK(x == m.end());
      m.insert(r);

      if (a == 12 && b == 22)
        CHECK_EQ(r, 298);
      if (a == 0 && b == 0)
        CHECK_EQ(r, 0);
    }

  env_t e;
  e["t_0_n"] = 12;
  e["t_1_n"] = 22;
  int s = parse(tg.size, e);
  CHECK_EQ(s, 299);

  /*
  std::cerr << *tg.off << '\n';
  cpp.print(tg.code);
  std::cerr << "=====\n";
  */
}

BOOST_AUTO_TEST_CASE(left_lin2) {
  std::vector<Table> v;
  Table t;
  t |= Table::LINEAR;
  t.set_sticky(Table::LEFT);
  t.set_left_rest(Yield::Size(Yield::Poly(0), Yield::Poly(2)));
  v.push_back(t);

  Table u;
  u |= Table::LINEAR;
  u.set_sticky(Table::LEFT);
  u.set_left_rest(Yield::Size(Yield::Poly(0), Yield::Poly(1)));
  v.push_back(u);

  Printer::Cpp cpp;

  Tablegen tg;

  tg.offset(0, v.begin(), v.end());

  set_t m;
  for (int c = 0; c < 3; ++c)
    for (int d = 0; d < 2; ++d)
  for (int a = 0; a < 13; ++a)
    for (int b = 0; b < 23; ++b) {
      env_t e;
      e["t_0_i"] = c;
      e["t_1_i"] = d;
      e["t_0_j"] = a;
      e["t_1_j"] = b;
      e["t_1_n"] = 22;
      int r = parse(tg.off, e);
      set_t::iterator x = m.find(r);
      CHECK(x == m.end());
      m.insert(r);

      if (c == 2 && d == 1 && a == 12 && b == 22)
        CHECK_EQ(r, 1793);
      if (c == 0 && d == 0 && a == 0 && b == 0)
        CHECK_EQ(r, 0);
    }

  env_t e;
  e["t_0_n"] = 12;
  e["t_1_n"] = 22;
  int s = parse(tg.size, e);
  CHECK_EQ(s, 1794);

  /*
  std::cerr << *tg.off << '\n';
  cpp.print(tg.code);
  std::cerr << "=====\n";
  */
}


BOOST_AUTO_TEST_CASE(right_lin) {
  std::vector<Table> v;
  Table t;
  t |= Table::LINEAR;
  t.set_sticky(Table::RIGHT);
  t.set_right_rest(Yield::Size(Yield::Poly(0), Yield::Poly(2)));
  v.push_back(t);


  Table u;
  u |= Table::LINEAR;
  u.set_sticky(Table::RIGHT);
  u.set_right_rest(Yield::Size(Yield::Poly(0), Yield::Poly(1)));
  v.push_back(u);

  Printer::Cpp cpp;

  Tablegen tg;

  tg.offset(0, v.begin(), v.end());

  set_t m;
  for (int c = 0; c < 3; ++c)
    for (int d = 0; d < 2; ++d)
  for (int a = 0; a < 13; ++a)
    for (int b = 0; b < 23; ++b) {
      env_t e;
      e["t_0_i"] = a;
      e["t_1_i"] = b;
      e["t_0_real_j"] = c;
      e["t_1_real_j"] = d;
      e["t_0_n"] = 12;
      e["t_1_n"] = 22;
      int r = parse(tg.off, e);
      set_t::iterator x = m.find(r);
      CHECK(x == m.end());
      m.insert(r);

      if (c == 2 && d == 1 && a == 12 && b == 22)
        CHECK_EQ(r, 1793);
      if (c == 0 && d == 0 && a == 0 && b == 0)
        CHECK_EQ(r, 0);
    }

  env_t e;
  e["t_0_n"] = 12;
  e["t_1_n"] = 22;
  int s = parse(tg.size, e);
  CHECK_EQ(s, 1794);

  /*
  std::cerr << *tg.off << '\n';
  cpp.print(tg.code);
  std::cerr << "=====\n";
  */
}

BOOST_AUTO_TEST_CASE(right_lin2) {
  std::vector<Table> v;
  Table t;
  t |= Table::LINEAR;
  t.set_sticky(Table::RIGHT);
  v.push_back(t);

  Table u;
  u |= Table::LINEAR;
  u.set_sticky(Table::RIGHT);
  v.push_back(u);

  Printer::Cpp cpp;

  Tablegen tg;

  tg.offset(0, v.begin(), v.end());

  set_t m;
  for (int a = 0; a < 13; ++a)
    for (int b = 0; b < 23; ++b) {
      env_t e;
      e["t_0_i"] = a;
      e["t_1_i"] = b;
      e["t_0_real_j"] = 0;
      e["t_1_real_j"] = 0;
      e["t_0_n"] = 12;
      e["t_1_n"] = 22;
      int r = parse(tg.off, e);
      set_t::iterator x = m.find(r);
      CHECK(x == m.end());
      m.insert(r);

      if (a == 12 && b == 22)
        CHECK_EQ(r, 298);
      if (a == 0 && b == 0)
        CHECK_EQ(r, 0);
    }

  env_t e;
  e["t_0_n"] = 12;
  e["t_1_n"] = 22;
  int s = parse(tg.size, e);
  CHECK_EQ(s, 299);

  /*
  std::cerr << *tg.off << '\n';
  cpp.print(tg.code);
  std::cerr << "=====\n";
  */
}

/* Stefan Janssen: I had to rename it to "my_quad", because gcc-4.5 at Solaris
   CeBiTec has some iso/math_c99.h include-fixed header file with the 
   name "quad" */
BOOST_AUTO_TEST_CASE(my_quad) {
  std::vector<Table> v;
  Table t;
  t |= Table::QUADRATIC;
  v.push_back(t);
  v.push_back(t);

  Printer::Cpp cpp;

  Tablegen tg;

  tg.offset(0, v.begin(), v.end());

  set_t m;
  for (int a = 0; a < 8; ++a)
    for (int b = 0; b < 5; ++b)
  for (int c = a; c < 8; ++c)
    for (int d = b; d < 5; ++d) {
      env_t e;
      e["t_0_i"] = a;
      e["t_0_j"] = c;
      e["t_1_i"] = b;
      e["t_1_j"] = d;
      e["t_0_n"] = 7;
      e["t_1_n"] = 4;


      int r = parse(tg.off, e);
      set_t::iterator x = m.find(r);
      CHECK(x == m.end());
      m.insert(r);

      if (c == 7 && d == 4 && a == 7 && b == 4)
        CHECK_EQ(r, 539);
      if (c == 0 && d == 0 && a == 0 && b == 0)
        CHECK_EQ(r, 0);
    }

  env_t e;
  e["t_0_n"] = 7;
  e["t_1_n"] = 4;
  int s = parse(tg.size, e);
  CHECK_EQ(s, 540);
}

BOOST_AUTO_TEST_CASE(diag) {
  std::vector<Table> v;
  Table t;
  t |= Table::QUADRATIC;
  v.push_back(t);

  Printer::Cpp cpp;

  Tablegen tg;

  tg.offset(0, v.begin(), v.end());

  set_t m;
  for (int a = 0; a < 13; ++a)
    for (int b = a; b < 13; ++b) {
      env_t e;
      e["t_0_i"] = a;
      e["t_0_j"] = b;
      e["t_0_n"] = 12;

      int r = parse(tg.off, e);
      set_t::iterator x = m.find(r);
      CHECK(x == m.end());
      m.insert(r);

      if (a == 12 && b == 12)
        CHECK_EQ(r, 90);
      if (a == 0 && b == 0)
        CHECK_EQ(r, 0);
    }

  env_t e;
  e["t_0_n"] = 12;
  int s = parse(tg.size, e);
  CHECK_EQ(s, 91);
}

BOOST_AUTO_TEST_CASE(mixed) {
  std::vector<Table> v;
  Table t;
  t |= Table::QUADRATIC;
  v.push_back(t);

  Table u;
  u |= Table::LINEAR;
  u.set_sticky(Table::RIGHT);
  v.push_back(u);


  Printer::Cpp cpp;

  Tablegen tg;

  tg.offset(0, v.begin(), v.end());

  set_t m;
  for (int c = 0; c < 8; ++c)
  for (int a = 0; a < 13; ++a)
    for (int b = a; b < 13; ++b) {
      env_t e;
      e["t_0_i"] = a;
      e["t_0_j"] = b;
      e["t_1_i"] = c;
      e["t_1_real_j"] = 0;
      e["t_0_n"] = 12;
      e["t_1_n"] = 7;

      int r = parse(tg.off, e);
      set_t::iterator x = m.find(r);
      CHECK(x == m.end());
      m.insert(r);

      if (c == 7 && a == 12 && b == 12)
        CHECK_EQ(r, 727);
      if (c == 0 && a == 0 && b == 0)
        CHECK_EQ(r, 0);
    }

  env_t e;
  e["t_0_n"] = 12;
  e["t_1_n"] = 7;
  int s = parse(tg.size, e);
  CHECK_EQ(s, 728);
}
