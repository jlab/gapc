#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE map_pool
#include <boost/test/unit_test.hpp>

#include "../../rtlib/map_pool.hh"

#include "macros.hh"

#include <iostream>

#include <boost/random/linear_congruential.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/tr1/random.hpp>
#include <ctime>

#include <vector>
#include <map>

void free_it(Map::Pool<size_t> &pool, std::vector<size_t*> &frees, std::map<size_t*, size_t> &map,
    size_t x)
{
  CHECK(map.find(frees[x]) != map.end());
  CHECK(map[frees[x]] == *frees[x]);
  map.erase(frees[x]);
  pool.free(frees[x]);
  frees[x] = 0;
}

typedef boost::mt19937 rand_gen;
typedef boost::uniform_int<> rand_dist;
BOOST_AUTO_TEST_CASE( random_alloc )
{
  rand_gen gen(static_cast<unsigned int>(std::time(0)));
  boost::variate_generator<rand_gen&, rand_dist>
    die_len(gen, rand_dist(0, 99));
  boost::variate_generator<rand_gen&, rand_dist>
    die_bin(gen, rand_dist(0, 1));

  std::vector<size_t*> frees;
  frees.resize(100);

  std::map<size_t*, size_t> map;

  Map::Pool<size_t> pool;

  for (size_t i = 0; i<100000; ++i) {
    unsigned int x = die_len();
    if (die_bin()) {
      if (frees[x]) {
        free_it(pool, frees, map, x);
      }
      frees[x] = pool.malloc();
      *frees[x] = die_len();
      CHECK(map.find(frees[x]) == map.end());
      map[frees[x]] = *frees[x];
    } else {
      if (frees[x]) {
        free_it(pool, frees, map, x);
      }
    }
  }

  for (size_t x = 0; x<frees.size(); ++x)
    if (frees[x])
      free_it(pool, frees, map, x);
}
