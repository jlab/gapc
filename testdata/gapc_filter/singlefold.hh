#ifndef SINGLEFOLD_HH
#define SINGLEFOLD_HH

#include "rnaoptions_defaults.hh"

template<typename alphabet, typename pos_type, typename T>
inline bool basepair(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j)
{
	return basepairing(seq, i, j);
}

#endif
