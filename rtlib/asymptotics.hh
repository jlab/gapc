#ifndef RTLIB_ASYMPTOTICS_HH_
#define RTLIB_ASYMPTOTICS_HH_

#include <cmath>

namespace Asymptotics
{

  template <typename T>
  T
  inline shape5(T length)
  {
    return std::pow(1.108, double(length));
  }
}

#endif  // RTLIB_ASYMPTOTICS_HH_
