/* 
 * File:   float_accuracy_operators.hh
 * Author: gatter
 *
 * Created on July 15, 2015, 9:28 AM
 */

#ifndef FLOAT_ACCURACY_OPERATORS_HH
#define	FLOAT_ACCURACY_OPERATORS_HH

#include <utility> 
#include <math.h>

extern const double depsilon;

template <class _T1>
inline bool operator==(const std::pair<_T1, double>& __x, const std::pair<_T1, double>& __y)
{  
  return __x.first == __y.first && std::fabs(__x.second - __y.second) < depsilon; 
}

template <class _T2>
inline bool operator==(const std::pair<double, _T2>& __x, const std::pair<double, _T2>& __y)
{ 
  return std::fabs(__x.first - __y.first) < depsilon && __x.second == __y.second; 
}

inline bool operator==(const std::pair<double, double>& __x, const std::pair<double, double>& __y)
{ 
  return std::fabs(__x.first - __y.first) < depsilon && std::fabs(__x.second - __y.second) < depsilon; 
}

#endif	/* FLOAT_ACCURACY_OPERATORS_HH */

