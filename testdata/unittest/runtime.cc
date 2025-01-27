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
#define BOOST_TEST_MODULE runtime
#include <boost/test/unit_test.hpp>

// include everything - no linking needed ...
// #define BOOST_TEST_MAIN
// #include <boost/test/included/unit_test_framework.hpp>

#include "macros.hh"

#include "../../src/runtime.hh"

#include "../../src/yieldsize.hh"
#include "../../src/table.hh"

BOOST_AUTO_TEST_CASE(runtime_asm) {
  Runtime::Asm::Poly a;
  CHECK_EQ(a, a);
  Runtime::Asm::Poly b(1);
  CHECK_EQ(b, b);
  CHECK_NOT_EQ(a, b);
  CHECK_GREATER(b, a);
  CHECK_GREATER(b, 42);
  b = Runtime::Asm::Poly(23);
  Runtime::Asm::Poly c(1);
  CHECK_NOT_EQ(b, c);
  CHECK_NOT_EQ(b, 42);

  c.set_exp();
  CHECK(c.is_exp());
  b = a;
  a.set_exp();
  CHECK_EQ(a, c);
  CHECK_NOT_EQ(b, a);
  b = Runtime::Asm::Poly(234223);
  CHECK_GREATER(a, b);
}

BOOST_AUTO_TEST_CASE(runtime_both) {
  Runtime::Asm::Poly a;
  Runtime::Poly b;
  CHECK_EQ(a, b);
  CHECK_EQ(b, a);
  a = Runtime::Asm::Poly(23);
  b = Runtime::Poly(1, 23);
  CHECK_EQ(b.degree(), static_cast<uint32_t>(23));
  CHECK_EQ(b, a);
  CHECK_EQ(a, b);
  b = Runtime::Poly(23, 3);
  a = Runtime::Asm::Poly(3);
  b *= a;
  Runtime::Poly c(23, 6);
  CHECK_EQ(c.degree(), b.degree());
  CHECK_EQ(c[6], b[6]);
  a.set_exp();
  c *= a;
  CHECK(c.is_exp());
  b = Runtime::Poly(3);
  a = Runtime::Asm::Poly(0);
  CHECK_EQ(b, a);
}


BOOST_AUTO_TEST_CASE(runtime_poly) {
  Runtime::Poly a;
  Runtime::Poly b(3);
  CHECK_EQ(b.degree(), a.degree());
  CHECK((a < b) || (a > b));
  CHECK(!(a < a) && !(a > a));
  CHECK(!(b < b) && !(b > b));
  CHECK_EQ(a, 0);
  CHECK_EQ(b, 3);
  CHECK_NOT_EQ(b, 0);
  CHECK_NOT_EQ(a, 3);

  a = Runtime::Poly(32, 3);
  CHECK_EQ(a.degree(), static_cast<uint32_t>(3));
  a.divide_by_n();
  CHECK_EQ(a.degree(), static_cast<uint32_t>(2));
  CHECK_EQ(false, a.is_exp());

  a = Runtime::Poly();
  b = Runtime::Poly(42, 32);
  b += Runtime::Poly(23, 43);
  b += Runtime::Poly(1);
  b.zero();
  CHECK(!(a < b) && !(a > b));

  a = Runtime::Poly(2, 5);
  a += Runtime::Poly(2, 0);
  a += Runtime::Poly(2, 1);
  a += Runtime::Poly(2);
  CHECK_GREATER(a, 23);

  a = Runtime::Poly(4, 3);
  b = Runtime::Poly(9, 7);
  a *= b;
  CHECK_EQ(a.degree(), static_cast<uint32_t>(10));
  CHECK_EQ(a[10], static_cast<uint32_t>(36));

  a = Runtime::Poly(2, 3);
  a += Runtime::Poly(3, 1);
  b = Runtime::Poly(3, 8);
  b += Runtime::Poly(4, 3);
  a *= b;
  CHECK_EQ(a.degree(), static_cast<uint32_t>(11));
  CHECK_EQ(a[4], static_cast<uint32_t>(12));
  CHECK_EQ(a[11], static_cast<uint32_t>(6));
  CHECK_EQ(a[9], static_cast<uint32_t>(9));
  CHECK_EQ(a[6], static_cast<uint32_t>(8));
  Runtime::Poly c = a;
  a = Runtime::Poly(2, 3);
  a += Runtime::Poly(3, 1);
  b = Runtime::Poly(3, 8);
  b += Runtime::Poly(4, 3);
  Runtime::Poly d = a * b;
  CHECK(!(d < c) && !(d > c));

  a |= b;
  CHECK(!(a < b) && !(a > b));
  a |= c;
  CHECK(!(a < c) && !(a > c));

  a = Runtime::Poly(3);
  b = Runtime::Poly(2);
  c = Runtime::Poly(0);
  for (int i=1; i < 4; i++)
    for (int j=1; j < 4; j++) {
      a += Runtime::Poly(i, j);
      b += Runtime::Poly(j, i);
      a *= b;
    }

/*
  13608n^19  + 286416n^18  + 2623104n^17  + 14221008n^16  + 52436160n^15  + 141792912n^14  + 294148944n^13  + 481753224n^12  + 634532544n^11  + 679739646n^10  + 595457646n^9  + 426630884n^8  + 248748476n^7  + 116783232n^6  + 43400560n^5  + 12443190n^4  + 2646798n^3  + 392100n^2  + 35992n + 1536
*/

  CHECK_EQ(a[19], 13608u);
  CHECK_EQ(a[18], 286416u);
  CHECK_EQ(a[17], 2623104u);
  CHECK_EQ(a[16], 14221008u);
  CHECK_EQ(a[15], 52436160u);
  CHECK_EQ(a[14], 141792912u);
  CHECK_EQ(a[13], 294148944u);
  CHECK_EQ(a[12], 481753224u);
  CHECK_EQ(a[11], 634532544u);
  CHECK_EQ(a[10], 679739646u);
  CHECK_EQ(a[9], 595457646u);
  CHECK_EQ(a[8], 426630884u);
  CHECK_EQ(a[7], 248748476u);
  CHECK_EQ(a[6], 116783232u);
  CHECK_EQ(a[5], 43400560u);
  CHECK_EQ(a[4], 12443190u);
  CHECK_EQ(a[3], 2646798u);
  CHECK_EQ(a[2], 392100u);
  CHECK_EQ(a[1], 35992u);
  CHECK_EQ(a[0], 1536u);

  c += Runtime::Poly(13608, 19);
  c += Runtime::Poly(286416, 18);
  c += Runtime::Poly(2623104, 17);
  c += Runtime::Poly(14221008, 16);
  c += Runtime::Poly(52436160, 15);
  c += Runtime::Poly(141792912, 14);
  c += Runtime::Poly(294148944, 13);
  c += Runtime::Poly(481753224, 12);
  c += Runtime::Poly(634532544, 11);
  c += Runtime::Poly(679739646, 10);
  c += Runtime::Poly(595457646, 9);
  c += Runtime::Poly(426630884, 8);
  c += Runtime::Poly(248748476, 7);
  c += Runtime::Poly(116783232, 6);
  c += Runtime::Poly(43400560, 5);
  c += Runtime::Poly(12443190, 4);
  c += Runtime::Poly(2646798, 3);
  c += Runtime::Poly(392100, 2);
  c += Runtime::Poly(35992, 1);
  c += Runtime::Poly(1536);

  CHECK(!(c < a)&&!(c > a));
}

BOOST_AUTO_TEST_CASE(runtime_saturate) {
  uint32_t i = 0;
  i--;
  CHECK_EQ(i, UINT32_MAX);
  Runtime::Poly a(i, 5);
  a += Runtime::Poly(1);
  CHECK_EQ(a[5], i);
  i -= 23;
  a = Runtime::Poly(i);
  a += Runtime::Poly(42);
  CHECK_EQ(a[0], UINT32_MAX);
  a += Runtime::Poly(UINT32_MAX, 5);
  a *= Runtime::Poly(i);
  CHECK_EQ(a[0], UINT32_MAX);
  CHECK_EQ(a[5], UINT32_MAX);
}

BOOST_AUTO_TEST_CASE(runtime_yield) {
  Runtime::Poly a(Yield::Poly(2342));
  CHECK_EQ(a, 2342);
  a = Runtime::Poly(Yield::Poly(Yield::UP));
  CHECK_EQ(a.degree(), 1u);
}

BOOST_AUTO_TEST_CASE(runtime_table) {
  Table t;
  t |= Table::QUADRATIC;
  Runtime::Poly a(t);
  CHECK_EQ(a.degree(), 2u);
  t.set_bounded(Yield::Poly(Yield::UP));
  a = Runtime::Poly(t);
  CHECK_EQ(a.degree(), 2u);
  t.set_bounded(Yield::Poly(23));
  a = Runtime::Poly(t);
  CHECK_EQ(a.degree(), 0u);
  t = Table();
  t |= Table::LINEAR;
  a = Runtime::Poly(t);
  CHECK_EQ(a.degree(), 1u);
  t.set_bounded(true);
  a = Runtime::Poly(t);
  CHECK_EQ(a.degree(), 0u);
  t = Table();
  t |= Table::CONSTANT;
  a = Runtime::Poly(t);
  CHECK_EQ(a, 1);
}

BOOST_AUTO_TEST_CASE(exponential) {
  Runtime::Asm::Poly x;
  x.set_exp();
  Runtime::Poly b;
  b *= x;
  Runtime::Poly c(23, 3);
  Runtime::Poly t;
  t = b * c;
  CHECK(t.is_exp());
  Runtime::Poly a;
  a.set(1, 0);
  a.set(23, 3);
  a *= x;
  CHECK_NOT_EQ(a, 1);
  CHECK_GREATER(a.degree(), static_cast<uint32_t>(3));
  a = 23;
  CHECK_EQ(a, 23);
  CHECK(!a.is_exp());
}

/* 
  Stefan Janssen: I had to rename it to "my_exp2", because gcc-4.5 at Solaris
  CeBiTec has some iso/math_c99.h include-fixed header file with the
  name "exp2"
*/
BOOST_AUTO_TEST_CASE(my_exp2) {
  Runtime::Poly a, x;
  Runtime::Asm::Poly t;
  t.set_exp();
  x *= t;
  CHECK(x.is_exp());
  a.set(23, 3);
  CHECK(a > x == false);
  CHECK(x < a == false);
}


