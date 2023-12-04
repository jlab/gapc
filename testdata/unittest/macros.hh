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

#ifndef TESTDATA_UNITTEST_MACROS_HH_
#define TESTDATA_UNITTEST_MACROS_HH_

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

// #define CHECK_NOT_EQ(L, R) BOOST_CHECK_PREDICATE(neq(), (L)(R))

// see also http://lists.boost.org/Archives/boost/2007/08/126826.php
#include <boost/lambda/lambda.hpp>
// using namespace boost::lambda;
#define CHECK_NOT_EQ(L, R) \
  BOOST_CHECK_PREDICATE( boost::lambda::_1 != boost::lambda::_2, (L)(R))

#define CHECK_LESS(L, R) \
  BOOST_CHECK_PREDICATE( boost::lambda::_1 < boost::lambda::_2, (L)(R))

#define CHECK_GREATER(L, R) \
  BOOST_CHECK_PREDICATE( boost::lambda::_1 > boost::lambda::_2, (L)(R))

#define CHECK_EQ(L, R) BOOST_CHECK_EQUAL(L, R)

#define CHECK(A) BOOST_CHECK(A)

#endif  // TESTDATA_UNITTEST_MACROS_HH_
