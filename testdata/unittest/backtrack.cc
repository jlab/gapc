// link against -lboost_unit_test_framework if boost/test/unit_test.hpp is
// used ...

// see also https://bugs.launchpad.net/ubuntu/+source/boost/+bug/162155
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE backtrack
#include <boost/test/unit_test.hpp>

// include everything - no linking needed ...
//#define BOOST_TEST_MAIN
//#include <boost/test/included/unit_test_framework.hpp>

#include "macros.hh"

#include "../../rtlib/backtrack.hh"

BOOST_AUTO_TEST_CASE( backtrack )
{
  Eval_List<int> *t = new Eval_List<int>();
  t->count = 1;
  intrusive_ptr<Eval_List<int> > l = t;
  CHECK_EQ(l->count, 2u);
  Eval_List<int> *u = new Eval_List<int>();
  u->count = 1;
  intrusive_ptr<Eval_List<int> > m(u);
  CHECK_EQ(u->count, 2u);
  l = m;
  CHECK_EQ(t->count, 1u);
  CHECK_EQ(u->count, 3u);

  l = t;
  CHECK_EQ(t->count, 2u);
  CHECK_EQ(u->count, 2u);
  m = 0;
  CHECK_EQ(u->count, 1u);
}


struct Base {
  int count;
  Base() : count(0) {}
};

struct A : virtual public Base {
  int a;
};

struct B : virtual public Base {
  int b;
};

struct Same : public A, public B { int s; };

BOOST_AUTO_TEST_CASE( inheritance )
{
  Same *s = new Same();
  A *a = s;
  B *b = s;
  Base *base = s;
  a->count = 23;
  CHECK_EQ(a->count, 23);
  CHECK_EQ(b->count, 23);
  CHECK_EQ(s->count, 23);
  CHECK_EQ(base->count, 23);
  CHECK_EQ(&a->count, &b->count);
  CHECK_EQ(&a->count, &s->count);
  CHECK_EQ(&a->count, &base->count);
}


