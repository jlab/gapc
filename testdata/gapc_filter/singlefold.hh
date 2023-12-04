#ifndef SINGLEFOLD_HH
#define SINGLEFOLD_HH

#include "rnaoptions_defaults.hh"

template<typename alphabet, typename pos_type, typename T>
inline bool basepair(const Basic_Sequence<alphabet, pos_type> &seq, T i, T j)
{
	return basepairing(seq, i, j);
}

//in the Vienna Package, iloop regions are restricted such that their _combined_ length, i.e. |left region| + |right region| cannot exceed 30 bases.
//our restrictions is usually more relaxed, because each region may have up to 30 bases individually, i.e. 60 bases for both regions in the worst case.
//with iloopSumMax we can enforce the Vienna behaviour
template<typename alphabet, typename pos_type, typename T>
inline bool iloopSumMax(int size, const Basic_Sequence<alphabet, pos_type> &seq, T lb_i, T lb_j, T lr_i, T lr_j, T x_i, T x_j, T rr_i, T rr_j, T rb_i, T rb_j) {
	assert(lr_i < lr_j);
	assert(rr_i < rr_j);
	return ((lr_j-lr_i) + (rr_j-rr_i)) <= unsigned(size);
}

#endif
