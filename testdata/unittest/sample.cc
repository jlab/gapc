#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#define BOOST_TEST_MODULE sample
#include <boost/test/unit_test.hpp>

#include "macros.hh"

#include "../../rtlib/sample.hh"

#include <cmath>

BOOST_AUTO_TEST_CASE( dists )
{
  //gsl_rng_env_setup();
  scil::rng rng;
  scil::ran_discrete x(rng);
  x.push_back(5);
  x.push_back(2);
  x.push_back(3);

  x.init();

  int array[3] = { 0 };
  for (size_t i = 0; i < 100000; ++i)
    array[x.sample()]++;

  CHECK_LESS(std::fabs(double(array[0])/50000.0-1.0), 0.01);
  CHECK_LESS(std::fabs(double(array[1])/20000.0-1.0), 0.01);
  CHECK_LESS(std::fabs(double(array[2])/30000.0-1.0), 0.01);
}

