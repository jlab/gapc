
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE vector_sparse
#include <boost/test/unit_test.hpp>
#include "macros.hh"
#include "../../rtlib/string.hh"
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

typedef std::pair<String, int> T;
bool stefan(const T &a, const T &b) {
  return a.second < b.second;
}
BOOST_AUTO_TEST_CASE( sorting ) {
  // std::sort fails in Vector_Sparse, I figure because the underlying array
  // is not reordered by the std::sort method, i.e. swap() is applied
  // incorrectly
  Vector_Sparse<T> ansvecsparse;
  ansvecsparse.resize(100);

  T ret_0;
  append(ret_0.first, "[]", 2);
  ret_0.second = -650;
  ansvecsparse.init(0, ret_0);

  T ret_1;
  append(ret_1.first, "[][]", 4);
  ret_1.second = -740;
  ansvecsparse.init(1, ret_1);

  T ret_2;
  append(ret_2.first, "[][][]", 6);
  ret_2.second = -370;
  ansvecsparse.init(2, ret_2);

  T ret_3;
  append(ret_3.first, "", 0);
  ret_3.second = 0;
  ansvecsparse.init(3, ret_3);

  T ret_4;
  append(ret_4.first, "[[][]]", 6);
  ret_4.second = -470;
  ansvecsparse.init(4, ret_4);

  T ret_5;
  append(ret_5.first, "[][][][]", 8);
  ret_5.second = 200;
  ansvecsparse.init(5, ret_5);

  T ret_6;
  append(ret_6.first, "[][[][]]", 8);
  ret_6.second = 410;
  ansvecsparse.init(6, ret_6);

  T ret_7;
  append(ret_7.first, "[[][]][]", 8);
  ret_7.second = 80;
  ansvecsparse.init(7, ret_7);

  T ret_8;
  append(ret_8.first, "[[][]][][]", 10);
  ret_8.second = 520;
  ansvecsparse.init(8, ret_8);

  T ret_9;
  append(ret_9.first, "[[][]][[][]]", 12);
  ret_9.second = 1450;
  ansvecsparse.init(9, ret_9);

  T ret_10;
  append(ret_10.first, "[[][]][][][]", 12);
  ret_10.second = 1540;
  ansvecsparse.init(10, ret_10);

  T ret_11;
  append(ret_11.first, "[[][][]][]", 10);
  ret_11.second = 330;
  ansvecsparse.init(11, ret_11);

  T ret_12;
  append(ret_12.first, "[[][][]]", 8);
  ret_12.second = -250;
  ansvecsparse.init(12, ret_12);

  T ret_13;
  append(ret_13.first, "[[][][]][][]", 12);
  ret_13.second = 1530;
  ansvecsparse.init(13, ret_13);

  T ret_14;
  append(ret_14.first, "[[[][]][]][]", 12);
  ret_14.second = 760;
  ansvecsparse.init(14, ret_14);

  T ret_15;
  append(ret_15.first, "[[[][]][]]", 10);
  ret_15.second = 420;
  ansvecsparse.init(15, ret_15);

  T ret_16;
  append(ret_16.first, "[[][][][]][]", 12);
  ret_16.second = 780;
  ansvecsparse.init(16, ret_16);

  T ret_17;
  append(ret_17.first, "[[][][][]]", 10);
  ret_17.second = 420;
  ansvecsparse.init(17, ret_17);

  T ret_18;
  append(ret_18.first, "[[][[][]]]", 10);
  ret_18.second = 440;
  ansvecsparse.init(18, ret_18);

  //prior sorting, first element should be [], -650
  CHECK_EQ(ansvecsparse(0).first, ret_0.first);
  CHECK_EQ(ansvecsparse(0).second, -650);
  //prior sorting, last element should be [[][[][]]], 440
  CHECK_EQ(ansvecsparse(18).first, ret_18.first);
  CHECK_EQ(ansvecsparse(18).second, 440);

  std::sort(ansvecsparse.begin(), ansvecsparse.end(), stefan);
  // after sorting, first element should be smallest, i.e. -740
  CHECK_EQ(ansvecsparse(0).second, -740);
  // after sorting, last element should be largest, i.e. 1540
  CHECK_EQ(ansvecsparse(18).second, 1540);

  // for (const auto& x : ansvecsparse) {
  //   std::cerr << x.second << ", " << x.first << "\n";
  // }
}
