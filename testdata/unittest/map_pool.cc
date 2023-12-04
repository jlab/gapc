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
#define BOOST_TEST_MODULE map_pool
#include <iostream>
#include <vector>
#include <map>
#include <ctime>
#include <boost/test/unit_test.hpp>

#include "../../rtlib/map_pool.hh"

#include "macros.hh"


#include <boost/random/linear_congruential.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/random.hpp>


void free_it(Map::Pool<size_t> *pool, std::vector<size_t*> *frees,
             std::map<size_t*, size_t> *map, size_t x) {
  CHECK(map->find(frees->operator[](x)) != map->end());
  CHECK(map->operator[](frees->operator[](x)) == *frees->operator[](x));
  map->erase(frees->operator[](x));
  pool->free(frees->operator[](x));
  frees->operator[](x) = 0;
}

typedef boost::mt19937 rand_gen;
typedef boost::uniform_int<> rand_dist;
BOOST_AUTO_TEST_CASE(random_alloc) {
  rand_gen gen(static_cast<unsigned int>(std::time(0)));
  boost::variate_generator<rand_gen&, rand_dist>
    die_len(gen, rand_dist(0, 99));
  boost::variate_generator<rand_gen&, rand_dist>
    die_bin(gen, rand_dist(0, 1));

  std::vector<size_t*> frees;
  frees.resize(100);

  std::map<size_t*, size_t> map;

  Map::Pool<size_t> pool;

  for (size_t i = 0; i < 100000; ++i) {
    unsigned int x = die_len();
    if (die_bin()) {
      if (frees[x]) {
        free_it(&pool, &frees, &map, x);
      }
      frees[x] = pool.malloc();
      *frees[x] = die_len();
      CHECK(map.find(frees[x]) == map.end());
      map[frees[x]] = *frees[x];
    } else {
      if (frees[x]) {
        free_it(&pool, &frees, &map, x);
      }
    }
  }

  for (size_t x = 0; x < frees.size(); ++x)
    if (frees[x])
      free_it(&pool, &frees, &map, x);
}
