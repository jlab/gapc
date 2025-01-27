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
#define BOOST_TEST_MODULE yieldsize
#include <boost/test/unit_test.hpp>

// include everything - no linking needed ...
// #define BOOST_TEST_MAIN
// #include <boost/test/included/unit_test_framework.hpp>

#include "macros.hh"

#include "../../src/yieldsize.hh"

BOOST_AUTO_TEST_CASE(yield_poly) {
  Yield::Poly p;
  Yield::Poly q(23);
  CHECK_EQ(p, p);
  CHECK_NOT_EQ(p, q);

  Yield::Poly z(0);
  Yield::Poly n(Yield::UP);
  CHECK_NOT_EQ(q, n);
  CHECK_EQ(p, z);
  CHECK_EQ(q.konst(), static_cast<uint32_t>(23));

  Yield::Poly nn;
  nn.set(Yield::UP);
  nn *= n;
  CHECK_EQ(nn, n);
  nn /= q;
  CHECK_EQ(nn, q);

  Yield::Poly r(42);
  Yield::Poly s = r;
  s += q;
  CHECK_EQ(s, 65);
  s += n;
  CHECK_EQ(s, n);

  CHECK_LESS(q, s);
  CHECK_GREATER(s, q);
  CHECK_GREATER(n, 462);
  CHECK_NOT_EQ(s, q);
  CHECK_LESS(q, n);
  CHECK_LESS(r, Yield::UP);

  s = 23;
  CHECK_EQ(s, q);

  s = n;
  CHECK_EQ(s, n);

  q = Yield::UP;
  CHECK_EQ(q, n);
  CHECK_NOT_EQ(r, Yield::Poly(Yield::UP));

  q.set(42);
  CHECK_EQ(q, r);
  CHECK_NOT_EQ(q, n);

  Yield::Poly a(23);
  Yield::Poly b(42);
  a /= b;
  CHECK_NOT_EQ(a, b);
  CHECK_EQ(a, 23);
  b /= a;
  CHECK_EQ(a, b);
  CHECK_EQ(b, 23);

  a.set(423);
  a *= n;
  CHECK_EQ(a, n);
  a.set(32);
  b.set(52);
  a *= b;
  CHECK_EQ(a, b);
  CHECK_EQ(a, 52);
}

BOOST_AUTO_TEST_CASE(yield_size) {
  Yield::Size a;
  CHECK_EQ(a, a);
  Yield::Size b(Yield::UP);
  CHECK_EQ(b, b);
  CHECK_NOT_EQ(a, b);

  a.set(23, 42);
  CHECK_NOT_EQ(a, b);
  a.set(23, Yield::UP);
  a.set(Yield::UP, 42);
  Yield::Size c(Yield::UP, 42);
  CHECK_EQ(a, c);
  a.set(Yield::UP, Yield::UP);
  CHECK_NOT_EQ(a, c);

  a.set(23, 41);
  b.set(42, 23);
  a+=b;
  c.set(65, 64);
  CHECK_EQ(a, c);
  b.set(1, Yield::UP);
  a+=b;
  CHECK_EQ(a.low(), 66);
  CHECK_EQ(a.high(), Yield::UP);
  a+=b;
  CHECK_EQ(a.low(), 67);
  CHECK_EQ(a.high(), Yield::UP);

  a /= b;
  CHECK_EQ(a, b);
  a /= c;
  CHECK_NOT_EQ(a, c);
  a.set(Yield::UP, Yield::UP);
  b.set(Yield::UP, Yield::UP);
  a /= b;
  CHECK_EQ(a.low(), Yield::UP);
  CHECK_EQ(a.high(), Yield::UP);
  a.set(23, Yield::UP);
  a /= b;
  CHECK_NOT_EQ(a.low(), Yield::Poly(Yield::UP));
  CHECK_EQ(a.high(), Yield::UP);
}
