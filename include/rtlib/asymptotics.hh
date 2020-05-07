#ifndef ASYMPTOTICS_HH
#define ASYMPTOTICS_HH

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

#endif
