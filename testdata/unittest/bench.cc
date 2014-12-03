#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE bench

#define GAPC_MOD_TRANSLATION_UNIT

#include <boost/test/unit_test.hpp>

#include "macros.hh"

#define STATS
#include "../../rtlib/bench.hh"

#include "../../rtlib/singleton.hh"

#include <time.h>

#include <iostream>
#include <sstream>

BOOST_AUTO_TEST_CASE( yield_poly )
{
  Singleton<Bench>::ref().add_event("start");
  struct timespec x = { 0 };
  x.tv_nsec = 500000000;
  nanosleep(&x, 0);

  //Singleton<Bench>::ref().add_event("backtrack");
  gapc::add_event("backtrack");

  x.tv_nsec = 0;
  x.tv_sec = 1;
  nanosleep(&x, 0);

  Singleton<Bench>::ref().add_event("stop");

  //gapc::print_events(std::cerr);

  std::ostringstream o;
  Singleton<Bench>::ref().put(o);
  std::string s = o.str();
  size_t a = s.find_first_of("0.333");
  CHECK(a != std::string::npos);
  size_t b = s.find_first_of("0.666");
  CHECK(b != std::string::npos);
}
