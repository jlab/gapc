#ifndef LOOK_HELPER_HH
#define LOOK_HELPER_HH

#ifndef MY_RATIONAL
#define MY_RATIONAL Rational
#endif

#include <cstdlib>

template<typename alphabet, typename pos_type>
inline MY_RATIONAL lookup(Rules rule, const Basic_Subsequence<alphabet, pos_type> &l,
                                 const Basic_Subsequence<alphabet, pos_type> &r)
{
  return lookup(rule, r.i-l.i);
}

inline MY_RATIONAL lookup(char c)
{
  switch (c) {
    case A_BASE : return (rule_prob[O_A][0]);
    case C_BASE : return (rule_prob[O_C][0]);
    case G_BASE : return (rule_prob[O_G][0]);
    case U_BASE : return (rule_prob[O_U][0]);
    default : std::abort();
  };
  return 0.0;
}

template<typename alphabet, typename pos_type>
inline MY_RATIONAL lookup_p1(char a, char b, const Basic_Subsequence<alphabet, pos_type> &l,
                                 const Basic_Subsequence<alphabet, pos_type> &r)
{
  switch (a) {
    case A_BASE :
      switch (b) {
        case U_BASE : return lookup(P_A_P_U, l, r);
        default : std::abort();
      }
    case C_BASE :
      switch (b) {
        case G_BASE : return lookup(P_C_P_G, l, r);
        default : std::abort();
      }
    case G_BASE :
      switch (b) {
        case C_BASE : return lookup(P_G_P_C, l, r);
        case U_BASE : return lookup(P_G_P_U, l, r);
        default : std::abort();
      }
    case U_BASE :
      switch (b) {
        case A_BASE : return lookup(P_U_P_A, l, r);
        case G_BASE : return lookup(P_U_P_G, l, r);
        default : std::abort();
      }
  }
  std::abort();
  return lookup(P_U_P_G, l, r);
}

template<typename alphabet, typename pos_type>
inline MY_RATIONAL lookup_p2(char a, char b, const Basic_Subsequence<alphabet, pos_type> &l,
                                        const Basic_Subsequence<alphabet, pos_type> &r)
{
  switch (a) {
    case A_BASE :
      switch (b) {
        case U_BASE : return lookup(P_A_R_U, l, r);
        default : std::abort();
      }
    case C_BASE :
      switch (b) {
        case G_BASE : return lookup(P_C_R_G, l, r);
        default : std::abort();
      }
    case G_BASE :
      switch (b) {
        case C_BASE : return lookup(P_G_R_C, l, r);
        case U_BASE : return lookup(P_G_R_U, l, r);
        default : std::abort();
      }
    case U_BASE :
      switch (b) {
        case A_BASE : return lookup(P_U_R_A, l, r);
        case G_BASE : return lookup(P_U_R_G, l, r);
        default : std::abort();
      }
  }
  std::abort();
  return lookup(P_U_R_G, l, r);
}

inline
double scale_l(unsigned i)
{
  assert(i);
  double ret = std::log(0.25);
  for (unsigned l = 1; l < i; ++l)
    ret += std::log(0.25);
  return ret;
}

inline
double scale_d(unsigned i)
{
  assert(i);
  //return 1.0/pow(2.0, i);
  //return 1.0/pow(1.25, i);
  return 1.0/pow(0.25, double(i));
  //return 1.0/pow(0.2, i);
}

#endif


