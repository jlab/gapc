#ifndef ISNTIMES_HH
#define ISNTIMES_HH

template<typename alphabet, typename pos_type, typename T>
inline bool isntimes(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j, alphabet x, unsigned int size)
{
  if (j<i)
    return false;

  if (j-i != size)
	return false;
  
  for (T k = i; k < j; k++) {
    if (seq[k] != x)
      return false;
  }
  
  return true;
}

//a filter to force that a parsed character corresponds to a nucleotide (and not a gap or some structure parts)
template<typename T>
inline bool isAnyBase(const Basic_Sequence<> &seq, T i, T j) {
  if (j<i) return false;

  return ((seq[i] == 'A')	|| (seq[i] == 'C') || (seq[i] == 'G') || (seq[i] == 'U') || (seq[i] == 'R') || (seq[i] == 'Y') || (seq[i] == 'M') || (seq[i] == 'K') || (seq[i] == 'W') || (seq[i] == 'B') || (seq[i] == 'D') || (seq[i] == 'H') || (seq[i] == 'V') || (seq[i] == 'N') || (seq[i] == 'S'));
}


#endif
