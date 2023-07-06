#ifndef ISNTIMES_HH
#define ISNTIMES_HH

template<typename alphabet, typename pos_type, typename T>
inline bool isntimes(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j,
                     alphabet x, unsigned int size) {
  if (j < i)
    return false;

  if (j-i != size)
    return false;

  for (T k = i; k < j; k++) {
    if (seq[k] != x)
      return false;
  }

  return true;
}

#endif
