#include <iostream>

#include <boost/date_time/posix_time/posix_time.hpp>

#include <unistd.h>

int main()
{
  boost::posix_time::ptime a = boost::posix_time::microsec_clock::universal_time();
  sleep(1);
  boost::posix_time::ptime b = boost::posix_time::microsec_clock::universal_time();
  sleep(1);
  boost::posix_time::ptime c = boost::posix_time::microsec_clock::universal_time();
  std::cout << a << ' ' << b << ' ' << (c-a) << ' ' << double((b-a).total_microseconds())/double((c-a).total_microseconds()) << std::endl;
}
