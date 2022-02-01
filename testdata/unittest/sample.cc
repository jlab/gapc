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

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE sample
#include <cmath>

#include <boost/test/unit_test.hpp>

#include "macros.hh"

#include "../../rtlib/sample.hh"


BOOST_AUTO_TEST_CASE(dists) {
  // gsl_rng_env_setup();
  scil::rng rng;
  scil::ran_discrete x(rng);
  x.push_back(5);
  x.push_back(2);
  x.push_back(3);

  x.init();

  int array[3] = { 0 };
  for (size_t i = 0; i < 100000; ++i)
    array[x.sample()]++;

  CHECK_LESS(std::fabs(static_cast<double>(array[0])/50000.0-1.0), 0.01);
  CHECK_LESS(std::fabs(static_cast<double>(array[1])/20000.0-1.0), 0.01);
  CHECK_LESS(std::fabs(static_cast<double>(array[2])/30000.0-1.0), 0.01);
}

