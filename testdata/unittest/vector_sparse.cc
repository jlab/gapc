
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE vector_sparse
#include <boost/test/unit_test.hpp>
#include "macros.hh"

#include "../../rtlib/vector_sparse.hh"


BOOST_AUTO_TEST_CASE( stapel )
{
  Stapel<size_t> stack;
  stack.resize(3);
  stack.push(23);
  stack.push(42);
  stack.push(187);
  const size_t t[3] = { 23, 42, 187 };
  size_t o[3] = { 0 };
  size_t j = 0;
  for (Stapel<size_t>::iterator i = stack.begin(); i != stack.end(); ++i, ++j)
    o[j] = *i;
  for (size_t i = 0; i < 3; ++i)
    CHECK_EQ(t[i], o[i]);
}

struct A {
  static size_t count;
  A()
  {
    ++count;
  }
  A(const A &a)
  {
    ++count;
  }
  ~A()
  {
    --count;
  }
};

size_t A::count = 0;

BOOST_AUTO_TEST_CASE( vector )
{
  A a;
  Vector_Sparse<A> *vector = new Vector_Sparse<A>();
  vector->resize(4096);
  Vector_Sparse<A> &v = *vector;
  v.init(23, a);
  v.init(42, a);
  v.init(187, a);
  CHECK_EQ(A::count, 4u);
  delete vector;
  CHECK_EQ(A::count, 1u);
}

BOOST_AUTO_TEST_CASE( vector2 )
{
  Vector_Sparse<size_t> v;
  v.resize(4096);
  v.init(23, 46);
  v.init(42, 84);
  v.init(187, 374);
  CHECK_EQ(v(23), 2u*23u);
  CHECK_EQ(v(42), 2u*42u);
  CHECK_EQ(v(187), 2u*187u);
}

BOOST_AUTO_TEST_CASE( reverse )
{
  Stapel<size_t> stack;
  stack.resize(3);
  stack.push(23);
  stack.push(42);
  stack.push(187);
  const size_t t[3] = { 187, 42, 23 };
  size_t o[3] = { 0 };
  size_t j = 0;
  for (Stapel<size_t>::reverse_iterator i = stack.rbegin(); i != stack.rend();
       ++i, ++j)
    o[j] = *i;
  for (size_t i = 0; i < 3; ++i)
    CHECK_EQ(t[i], o[i]);
}

BOOST_AUTO_TEST_CASE( vector_itr )
{
  Vector_Sparse<size_t> v;
  v.resize(4096);
  v.init(23, 46);
  v.init(42, 84);
  v.init(187, 374);
  size_t j = 0;
  const size_t a[3] = { 46, 84, 374 };
  size_t t[3] = { 0 };
  for (Vector_Sparse<size_t>::iterator i = v.begin(); i != v.end(); ++i, ++j)
    t[j] = *i;
  for (size_t i = 0; i < 3; ++i)
    CHECK_EQ(a[i], t[i]);
}

