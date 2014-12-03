
#ifndef INTERACT_HH
#define INTERACT_HH

template<typename a1, typename a2, typename pos_type, typename T>
inline bool basepair(const Basic_Sequence<a1, pos_type> &s1,
    const Basic_Sequence<a2, pos_type> &s2,
    T i1, T j1,
    T i2, T j2)
{
  assert(i1<=j1);
  assert(i2<=j2);

  if (j1-i1 == 0 || j2-i2 == 0)
    return false;

  char a = lower_case(s1[i1]);
  char b = lower_case(s2[i2]);

  switch (a) {
    case 'a' :
      switch (b) {
        case 'u' : return true;
        case 't' : return true;
      }
      break;
    case 'u' :
      switch (b) {
        case 'a' : return true;
        case 'g' : return true;
      }
      break;
    case 't' :
      switch (b) {
        case 'a' : return true;
      }
      break;
    case 'g' :
      switch (b) {
        case 'c' : return true;
        case 'u' : return true;
      }
      break;
    case 'c' :
      switch (b) {
        case 'g' : return true;
      }
      break;
  }
  return false;

}

#endif
