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
#define BOOST_TEST_MODULE sequence
#include <string.h>
#include <string>
#include <boost/test/unit_test.hpp>

// include everything - no linking needed ...
// #define BOOST_TEST_MAIN
// #include <boost/test/included/unit_test_framework.hpp>

#include "macros.hh"
#include "../../rtlib/sequence.hh"


BOOST_AUTO_TEST_CASE(copier) {
  Basic_Sequence<double> s;
  const char *t = "0.1 0.3 0.4";
  s.copy(t, strlen(t));
  CHECK_EQ(s.size(), 3);
  CHECK_EQ(s[0], 0.1);
  CHECK_EQ(s[1], 0.3);
  CHECK_EQ(s[2], 0.4);
}

#include "../../rtlib/terminal.hh"

BOOST_AUTO_TEST_CASE(sepp) {
  Basic_Sequence<double> s;
  const char *t = "0.23 nan 4.2";
  s.copy(t, strlen(t));
  CHECK_EQ(s.size(), 3);
  double x = CHAR_SEP(s, 1, 2);
  CHECK_NOT_EQ(x, x);
}

#include "../../rtlib/empty.hh"

BOOST_AUTO_TEST_CASE(nann) {
  Basic_Sequence<double> s;
  const char *t = "0.23 nan 4.2";
  s.copy(t, strlen(t));
  CHECK_EQ(s.size(), 3);
  double x = NON(s, 1, 2);
  CHECK_NOT_EQ(x, x);
  double y = NON(s, 0, 1);
  CHECK(isEmpty(y));
}

BOOST_AUTO_TEST_CASE(multichar) {
  const char inp[] = "acgu#ugca#aucg#";
  Basic_Sequence<M_Char> s;
  s.copy(inp, strlen(inp));
  CHECK_EQ(s.rows(), 3);
  CHECK_EQ(s.size(), 4);
  char *out = new char[16]();
  unsigned x = 0;
  for (unsigned r = 0; r < s.rows(); ++r) {
    char *a = s.row(r);
    for (unsigned i = 0; i < s.size(); ++i)
      out[x++] = a[i];
    out[x++] = '#';
  }
  CHECK_EQ(inp, out);
  delete[] out;

  const char cmp[] = "aua|cgu|gcc|uag|";
  out = new char[17]();
  x = 0;
  for (unsigned i = 0; i < s.size(); ++i) {
    M_Char m = s[i];
    for (unsigned r = 0; r < s.rows(); ++r)
      out[x++] = m.column(r);
    out[x++] = '|';
  }
  CHECK_EQ(cmp, out);
  delete[] out;
}

BOOST_AUTO_TEST_CASE(multichar_exp) {
  const char inp[] = "acgu#ugca#acg#";
  Basic_Sequence<M_Char> s;
  bool b = false;
  try {
    s.copy(inp, strlen(inp));
  } catch (const std::exception &e) {
    b = true;
  }
  CHECK(b);
}

#include "rna.hh"

BOOST_AUTO_TEST_CASE(multichar_conv) {
  Basic_Sequence<M_Char> s;
  const char inp[] = "acgu#u_ca#auc_#";
  s.copy(inp, strlen(inp));
  char_to_rna(s);

  CHECK_EQ(s.rows(), 3);
  CHECK_EQ(s.size(), 4);

  char *out = new char[16]();
  unsigned x = 0;
  for (unsigned r = 0; r < s.rows(); ++r) {
    char *a = s.row(r);
    for (unsigned i = 0; i < s.size(); ++i)
      out[x++] = a[i];
    out[x++] = '#';
  }
  const char cmp0[] = "\1\2\3\4#\4\5\2\1#\1\4\2\5#";
  CHECK_EQ(cmp0, out);
  delete[] out;

  const char cmp[] = "\1\4\1|\2\5\4|\3\2\2|\4\1\5|";
  out = new char[17]();
  x = 0;
  for (unsigned i = 0; i < s.size(); ++i) {
    M_Char m = s[i];
    for (unsigned r = 0; r < s.rows(); ++r)
      out[x++] = m.column(r);
    out[x++] = '|';
  }
  CHECK_EQ(cmp, out);
  delete[] out;
}

#include "../../rtlib/subsequence.hh"


BOOST_AUTO_TEST_CASE(seq_char_test) {
  Sequence s;
  const char inp[] = "acgu#u_ca#auc_#";
  s.copy(inp, strlen(inp));
  Subsequence sub(s, 3, 5);
  CHECK_EQ(seq_char(sub, 1), 'c');
}

BOOST_AUTO_TEST_CASE(seq_upper) {
  Sequence s;
  const char inp[] = "foobarbaz";
  s.copy(inp, strlen(inp));
  char_to_upper(s);
  std::string x;
  for (Sequence::iterator i = s.begin(); i != s.end(); ++i)
    x.push_back(*i);
  CHECK_EQ(x, "FOOBARBAZ");
}

