// link against -lboost_unit_test_framework if boost/test/unit_test.hpp is
// used ...

// see also https://bugs.launchpad.net/ubuntu/+source/boost/+bug/162155
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE runtime
#include <boost/test/unit_test.hpp>

// include everything - no linking needed ...
//#define BOOST_TEST_MAIN
//#include <boost/test/included/unit_test_framework.hpp>

#include "macros.hh"

#include "../../src/runtime.hh"

#include "../../src/yieldsize.hh"
#include "../../src/table.hh"

BOOST_AUTO_TEST_CASE( runtime_asm )
{
  using namespace Runtime;
  Asm::Poly a;
  CHECK_EQ(a, a);
  Asm::Poly b(1);
  CHECK_EQ(b, b);
  CHECK_NOT_EQ(a, b);
  CHECK_GREATER(b, a);
  CHECK_GREATER(b, 42);
  b = Asm::Poly(23);
  Asm::Poly c(1);
  CHECK_NOT_EQ(b, c);
  CHECK_NOT_EQ(b, 42);

  c.set_exp();
  CHECK(c.is_exp());
  b = a;
  a.set_exp();
  CHECK_EQ(a, c);
  CHECK_NOT_EQ(b, a);
  b = Asm::Poly(234223);
  CHECK_GREATER(a, b);
}

BOOST_AUTO_TEST_CASE( runtime_both )
{
  using namespace Runtime;
  Asm::Poly a;
  Poly b;
  CHECK_EQ(a, b);
  CHECK_EQ(b, a);
  a = Asm::Poly(23);
  b = Poly(1,23);
  CHECK_EQ(b.degree(), uint32_t(23));
  CHECK_EQ(b, a);
  CHECK_EQ(a, b);
  b = Poly(23, 3);
  a = Asm::Poly(3);
  b *= a;
  Poly c(23, 6);
  CHECK_EQ(c.degree(), b.degree());
  CHECK_EQ(c[6], b[6]);
  a.set_exp();
  c *= a;
  CHECK(c.is_exp());
  b = Poly(3);
  a = Asm::Poly(0);
  CHECK_EQ(b, a);
}


BOOST_AUTO_TEST_CASE( runtime_poly )
{
  using namespace Runtime;
  Poly a;
  Poly b(3);
  CHECK_EQ(b.degree(), a.degree());
  CHECK((a<b) || (a>b));
  CHECK(!(a<a) && !(a>a));
  CHECK(!(b<b) && !(b>b));
  CHECK_EQ(a, 0);
  CHECK_EQ(b, 3);
  CHECK_NOT_EQ(b, 0);
  CHECK_NOT_EQ(a, 3);
  
  a = Poly(32, 3);
  CHECK_EQ(a.degree(), uint32_t(3));
  a.divide_by_n();
  CHECK_EQ(a.degree(), uint32_t(2));
  CHECK_EQ(false, a.is_exp());

  a = Poly();
  b = Poly(42, 32);
  b += Poly(23,43);
  b += 1;
  b.zero();
  CHECK(!(a<b) && !(a>b));

  a = Poly(2,5);
  a += Poly(2,0);
  a += Poly(2,1);
  a += Poly(2);
  CHECK_GREATER(a, 23);

  a = Poly(4, 3);
  b = Poly(9, 7);
  a *= b;
  CHECK_EQ(a.degree(), uint32_t(10));
  CHECK_EQ(a[10], uint32_t(36));

  a = Poly(2, 3);
  a += Poly(3, 1);
  b = Poly(3, 8);
  b += Poly(4, 3);
  a *= b;
  CHECK_EQ(a.degree(), uint32_t(11));
  CHECK_EQ(a[4], uint32_t(12));
  CHECK_EQ(a[11], uint32_t(6));
  CHECK_EQ(a[9], uint32_t(9));
  CHECK_EQ(a[6], uint32_t(8));
  Poly c = a;
  a = Poly(2, 3);
  a += Poly(3, 1);
  b = Poly(3, 8);
  b += Poly(4, 3);
  Poly d = a * b;
  CHECK(!(d<c) && !(d>c));

  a |= b;
  CHECK(!(a<b) && !(a>b));
  a |= c;
  CHECK(!(a<c) && !(a>c));

  a = Poly(3);
  b = Poly(2);
  c = Poly(0);
  for (int i=1; i<4; i++)
    for (int j=1; j<4; j++) {
      a += Poly(i,j);
      b += Poly(j,i);
      a *= b;
    }

//      13608n^19  + 286416n^18  + 2623104n^17  + 14221008n^16  + 52436160n^15  + 141792912n^14  + 294148944n^13  + 481753224n^12  + 634532544n^11  + 679739646n^10  + 595457646n^9  + 426630884n^8  + 248748476n^7  + 116783232n^6  + 43400560n^5  + 12443190n^4  + 2646798n^3  + 392100n^2  + 35992n + 1536

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

      c += Poly(13608, 19);
c += Poly(286416, 18);
c += Poly(2623104, 17);
c += Poly(14221008, 16);
c += Poly(52436160, 15);
c += Poly(141792912, 14);
c += Poly(294148944, 13);
c += Poly(481753224, 12);
c += Poly(634532544, 11);
c += Poly(679739646, 10);
c += Poly(595457646, 9);
c += Poly(426630884, 8);
c += Poly(248748476, 7);
c += Poly(116783232, 6);
c += Poly(43400560, 5);
c += Poly(12443190, 4);
c += Poly(2646798, 3);
c += Poly(392100, 2);
c += Poly(35992, 1);
c += 1536;

  CHECK(!(c<a)&&!(c>a));



}

BOOST_AUTO_TEST_CASE( runtime_saturate)
{
  using namespace Runtime;
  uint32_t i = 0;
  i--;
  CHECK_EQ(i, UINT32_MAX);
  Poly a(i, 5);
  a += 1;
  CHECK_EQ(a[5], i);
  i -= 23;
  a = Poly(i);
  a += 42;
  CHECK_EQ(a[0], UINT32_MAX);
  a += Poly(UINT32_MAX, 5);
  a *= Poly(i);
  CHECK_EQ(a[0], UINT32_MAX);
  CHECK_EQ(a[5], UINT32_MAX);
}

BOOST_AUTO_TEST_CASE( runtime_yield )
{
  using namespace Runtime;
  Poly a(Yield::Poly(2342));
  CHECK_EQ(a, 2342);
  a = Poly(Yield::Poly(Yield::UP));
  CHECK_EQ(a.degree(), 1u);
}

BOOST_AUTO_TEST_CASE( runtime_table )
{
  using namespace Runtime;
  Table t;
  t |= Table::QUADRATIC;
  Poly a(t);
  CHECK_EQ(a.degree(), 2u);
  t.set_bounded(Yield::Poly(Yield::UP));
  a = Poly(t);
  CHECK_EQ(a.degree(), 2u);
  t.set_bounded(Yield::Poly(23));
  a = Poly(t);
  CHECK_EQ(a.degree(), 0u);
  t = Table();
  t |= Table::LINEAR;
  a = Poly(t);
  CHECK_EQ(a.degree(), 1u);
  t.set_bounded(true);
  a = Poly(t);
  CHECK_EQ(a.degree(), 0u);
  t = Table();
  t |= Table::CONSTANT;
  a = Poly(t);
  CHECK_EQ(a, 1);
}

BOOST_AUTO_TEST_CASE( exponential )
{
  using namespace Runtime;
  Asm::Poly x;
  x.set_exp();
  Poly b;
  b *= x;
  Poly c(23, 3);
  Poly t;
  t = b * c;
  CHECK(t.is_exp());
  Poly a;
  a.set(1, 0);
  a.set(23, 3);
  a *= x;
  CHECK_NOT_EQ(a, 1);
  CHECK_GREATER(a.degree(), uint32_t(3));
  a = 23;
  CHECK_EQ(a, 23);
  CHECK(!a.is_exp());
}

//Stefan Janssen: I had to rename it to "my_exp2", because gcc-4.5 at Solaris CeBiTec has some iso/math_c99.h include-fixed header file with the name "exp2"
BOOST_AUTO_TEST_CASE ( my_exp2 )
{
  using namespace Runtime;


  Poly a, x;
  Asm::Poly t;
  t.set_exp();
  x *= t;
  CHECK(x.is_exp());
  a.set(23, 3);
  CHECK(a > x == false);
  CHECK(x < a == false);
}


