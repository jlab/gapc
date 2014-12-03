#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE Template Order
#include <boost/test/unit_test.hpp>

#include "macros.hh"

#include <iostream>

struct Foo {
  int a;
  char b;
};

template<typename T> inline int match(const T &x)
{
  return 23;
}

//inline int match(const int &x);

template <typename T> inline int not_match(const T &x)
{
  return match(x) + 1;
}

/*
inline int match(const int &x)
//inline int match(int x)
{
  return 42;
}
*/

template <> inline int match<int>(const int &x)
{
  return 42;
}

inline int match(const Foo & x)
{
  return 52;
}

inline int match(const double &d)
{
  return 62;
}


BOOST_AUTO_TEST_CASE( empty )
{
  int i = 0;
  CHECK_EQ(not_match(i), 43);
  char c = 0;
  Foo f;
  CHECK_EQ(not_match(f), 53);
  double d = 0;
  // WTF?!?
  // gcc: true Sun CC: false
  // http://gcc.gnu.org/bugzilla/show_bug.cgi?id=45132
  //CHECK_NOT_EQ(not_match(d), 63);
}


