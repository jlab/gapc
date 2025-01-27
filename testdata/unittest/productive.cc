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
#define BOOST_TEST_MODULE productive
#include <sstream>
#include <string>
#include <boost/test/unit_test.hpp>

// include everything - no linking needed ...
// #define BOOST_TEST_MAIN
// #include <boost/test/included/unit_test_framework.hpp>

#include "macros.hh"

#include "../../src/driver.hh"
#include "../../src/log.hh"

BOOST_AUTO_TEST_CASE(twotrack) {
  std::ostringstream o;
  Log log;
  log.set_debug();
  log.set_ostream(o);
  Driver driver;
  std::string filename = std::string("../grammar/nonproductive.2track");
  driver.setFilename(filename);
  driver.parse();
  CHECK(!driver.is_failing());

  Grammar *grammar = driver.ast.grammar();

  // see grammar->check_semantic();
  bool b, r = true;
  b = grammar->init_tabulated();
  r = r && b;
  b = grammar->init_axiom();
  CHECK_EQ(b, true);
  r = r && b;
  r = grammar->init_nt_links();
  r = r && b;
  grammar->remove_unreachable();
  CHECK_EQ(r, true);
  b = !grammar->has_nonproductive_NTs();
  r = r && b;
  CHECK_EQ(r, false);

  std::string x(o.str());
  CHECK(x.find("Nonterminal fold is non-productive") != std::string::npos);
}


