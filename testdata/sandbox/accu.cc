
#include <boost/cstdint.hpp>

#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/mean.hpp>

#include <iostream>

int main()
{
  namespace ba = boost::accumulators;
  ba::accumulator_set<uint32_t, ba::stats<ba::tag::sum> > s;
  s(uint32_t(1) << 31);
  std::cerr << ba::sum(s) << std::endl;
  s(uint32_t(1) << 31);
  std::cerr << ba::sum(s) << std::endl;
  return 0;
}
