#ifndef CHOENER_HH
#define CHOENER_HH

#include <cassert>

#define lookup(x, a, c, g, u, ax, cx, gx, ux) \
  lookupT<a, c, g, u>(x, ax, cx, gx, ux)

template <char a, char c, char g, char u, typename T>
inline
T lookupT(char x, T ax, T cx, T gx, T ux)
{ 
  switch (x) { 
    case a : return ax; 
    case c : return cx; 
    case g : return gx; 
    case u : return ux; 
  } 
  assert(23 == 0); 
}

#define lookup2(x, y,\
    a, c, g, u,\
    at, ct, gt, ut,\
    aa, ac, ag, au,\
    ca, cc, cg, cu,\
    ga, gc, gg, gu,\
    ua, uc, ug, uu)\
  lookup2T<a, c, g, u, at, ct, gt, ut>(x, y, \
    aa, ac, ag, au,\
    ca, cc, cg, cu,\
    ga, gc, gg, gu,\
    ua, uc, ug, uu)\

template <char a, char c, char g, char u,
char at, char ct, char gt, char ut,
typename T>
inline
T lookup2T(char x, char y,
    T aa, T ac, T ag, T au,
    T ca, T cc, T cg, T cu,
    T ga, T gc, T gg, T gu,
    T ua, T uc, T ug, T uu)
{
  switch (x) {
    case a :
      switch (y) {
        case at : return aa;
        case ct : return ac;
        case gt : return ag;
        case ut : return au;
      }
      assert(42 == 0);
    case c :
      switch (y) {
        case at : return ca;
        case ct : return cc;
        case gt : return cg;
        case ut : return cu;
      }
      assert(43 == 0);
    case g :
      switch (y) {
        case at : return ga;
        case ct : return gc;
        case gt : return gg;
        case ut : return gu;
      }
      assert(44 == 0);
    case u :
      switch (y) {
        case at : return ua;
        case ct : return uc;
        case gt : return ug;
        case ut : return uu;
      }
      assert(45 == 0);
  }
  assert(46 == 0);
}

#include "rtlib/cm_alph.hh"

#ifdef __APPLE__
  // work around weird Mac OS X type ambiguity problems
  // cf. http://stackoverflow.com/questions/11603818/why-is-there-ambiguity-between-uint32-t-and-uint64-t-when-using-size-t-on-mac-os

  #ifdef __x86_64__
    typedef uint64_t mySize;
  #else
    typedef uint32_t mySize;
  #endif
#else
  typedef size_t mySize;
#endif

typedef Fiber<mySize, unsigned char, CmAlph<mySize, unsigned char> > str;

inline str pushD(str x)
{
  return push_after_front(x, 'I', 'D');
}

inline str pushIR(str x)
{
  return push_before_back(x, 'D', 'I');
}

#endif
