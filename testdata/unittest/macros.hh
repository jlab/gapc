#ifndef MACROS_HH
#define MACROS_HH

#include <boost/test/test_tools.hpp>

/*
struct neq {
  template <typename Left, typename Right>
  bool
  operator()( Left const& left, Right const& right ) const
  {
    return left != right;
  }
};
*/

//#define CHECK_NOT_EQ(L, R) BOOST_CHECK_PREDICATE(neq(), (L)(R))

// see also http://lists.boost.org/Archives/boost/2007/08/126826.php
#include <boost/lambda/lambda.hpp>
using namespace boost::lambda;
#define CHECK_NOT_EQ(L, R) \
  BOOST_CHECK_PREDICATE( _1 != _2, (L)(R))

#define CHECK_LESS(L, R) \
  BOOST_CHECK_PREDICATE( _1 < _2, (L)(R))

#define CHECK_GREATER(L, R) \
  BOOST_CHECK_PREDICATE( _1 > _2, (L)(R))

#define CHECK_EQ(L, R) BOOST_CHECK_EQUAL(L, R)

#define CHECK(A) BOOST_CHECK(A)

#endif
