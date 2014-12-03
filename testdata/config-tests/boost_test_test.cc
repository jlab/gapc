#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE boost_test_test
#include <boost/test/unit_test.hpp>

#include <boost/lambda/lambda.hpp>
using namespace boost::lambda;
#define CHECK_EQ(L, R) BOOST_CHECK_EQUAL(L, R)
#define CHECK_NOT_EQ(L, R) \
  BOOST_CHECK_PREDICATE( _1 != _2, (L)(R))

BOOST_AUTO_TEST_CASE( xyz )
{
  bool a = true;
  CHECK_EQ(a, a);
  CHECK_NOT_EQ(true, false);
}
