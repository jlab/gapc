#ifndef typesRNAfolding_hh
#define typesRNAfolding_hh

struct answer_pknot_mfe {
	int energy;
	int betaLeftOuter;
	int alphaRightOuter;
	bool empty_;
	
	answer_pknot_mfe() : empty_(false) {
	}
	
	bool operator>(const answer_pknot_mfe& other) const { 
		return energy > other.energy; 
	}
	
	bool operator<(const answer_pknot_mfe& other) const { 
		return energy < other.energy; 
	}
	
	bool operator==(const answer_pknot_mfe& other) const { 
		return energy == other.energy; 
	}
	
	template <typename T> 
	bool operator>(const T &other) const {
		return energy > other; 
	}
	
	template <typename T> 
	bool operator<(const T &other) const {
		return energy < other; 
	}
	
	template <typename T> 
	bool operator==(const T &other) const {
		return energy == other; 
	}

	answer_pknot_mfe(int i) : energy(i), empty_(false) {
	}
	
	answer_pknot_mfe operator+(const answer_pknot_mfe &other) const {
		assert(!empty_); 
		assert(!other.empty_);
		return answer_pknot_mfe(energy + other.energy);
	}

	answer_pknot_mfe operator-(const answer_pknot_mfe &other) const {
		assert(!empty_);
		if (other.empty_) {
			return answer_pknot_mfe(energy);
		}
		return answer_pknot_mfe(energy - other.energy);
	}
	
	bool operator<=(const answer_pknot_mfe& other) const {
		assert(!empty_); 
		assert(!other.empty_);
		return energy <= other.energy;
	}
};

inline std::ostream &operator<<(std::ostream &o, const answer_pknot_mfe &tuple) {
	o << '('   << tuple.energy   << ", " << tuple.betaLeftOuter << ", " << tuple.alphaRightOuter << ')' ;
	return o;
}

inline void empty(answer_pknot_mfe &e) {
	e.empty_ = true; 
}

inline bool isEmpty(const answer_pknot_mfe &e) {
	return e.empty_; 
}

inline uint32_t hashable_value(const answer_pknot_mfe& candidate) {
  return candidate.energy; // + candidate.betaLeftOuter + candidate.alphaRightOuter; // for backtracing: mfe values must be unique, e.g. there cannot be two candidates with -2.0 kcal/mol but different betaLeftOuter / alphaRightOuter values
}



struct answer_pknot_pfunc {
	double pfunc;
	int betaLeftOuter;
	int alphaRightOuter;
	bool empty_;
	
	answer_pknot_pfunc() : empty_(false) {
	}
	
	bool operator>(const answer_pknot_pfunc& other) const { 
		return pfunc > other.pfunc; 
	}
	
	bool operator<(const answer_pknot_pfunc& other) const { 
		return pfunc < other.pfunc; 
	}
	
	bool operator==(const answer_pknot_pfunc& other) const { 
		return pfunc == other.pfunc; 
	}
	
	template <typename T> 
		bool operator>(const T &other) const {
		return pfunc > other; 
	}
	
	template <typename T> 
		bool operator<(const T &other) const {
		return pfunc < other; 
	}
	
	template <typename T> 
		bool operator==(const T &other) const {
		return pfunc == other; 
	}

	answer_pknot_pfunc(int i) : pfunc(i), empty_(false) {
	}
	
	answer_pknot_pfunc operator+(const answer_pknot_pfunc &other) const {
		assert(!empty_); 
		assert(!other.empty_);
		return answer_pknot_pfunc(pfunc + other.pfunc);
	}
	
	answer_pknot_pfunc operator-(const answer_pknot_pfunc &other) const {
		assert(!empty_);
		if (other.empty_) {
			return answer_pknot_pfunc(pfunc);
		}
		return answer_pknot_pfunc(pfunc - other.pfunc);
	}
	
	bool operator<=(const answer_pknot_pfunc& other) const {
		assert(!empty_); 
		assert(!other.empty_);
		return pfunc <= other.pfunc;
	}
};

inline std::ostream &operator<<(std::ostream &o, const answer_pknot_pfunc &tuple) {
	o << '('   << tuple.pfunc   << ", " << tuple.betaLeftOuter << ", " << tuple.alphaRightOuter << ')' ;
	return o;
}

inline void empty(answer_pknot_pfunc &e) {
	e.empty_ = true; 
}

inline bool isEmpty(const answer_pknot_pfunc &e) {
	return e.empty_; 
}

inline answer_pknot_pfunc& operator+=(answer_pknot_pfunc &a, const answer_pknot_pfunc &b) {
	assert(!a.empty_); assert(!b.empty_);
	a.pfunc += b.pfunc;
	return a;
}

inline double operator+=(double a, const answer_pknot_pfunc &b) {
	assert(!b.empty_);
	a += b.pfunc;
	return a;
}


struct answer_macrostate_mfe {
	int energy;
	TUSubsequence firstStem;
	TUSubsequence lastStem;
	bool empty_;
	answer_macrostate_mfe() : empty_(false) {
	}

	bool operator>(const answer_macrostate_mfe& other) const {
		return energy > other.energy;
	}

	bool operator<(const answer_macrostate_mfe& other) const {
		return energy < other.energy;
	}

	bool operator==(const answer_macrostate_mfe& other) const {
		return energy == other.energy;
	}

	template<typename T>
	bool operator>(const T &other) const {
		return energy > other;
	}

	template<typename T>
	bool operator<(const T &other) const {
		return energy < other;
	}

	template<typename T>
	bool operator==(const T &other) const {
		return energy == other;
	}

	answer_macrostate_mfe(int i) : energy(i), empty_(false) {
	}

	answer_macrostate_mfe operator+(const answer_macrostate_mfe &other) const {
		assert(!empty_);
		assert(!other.empty_);
		return answer_macrostate_mfe(energy + other.energy);
	}

	answer_macrostate_mfe operator-(const answer_macrostate_mfe &other) const {
		assert(!empty_);
		if (other.empty_) {
			return answer_macrostate_mfe(energy);
		}
		return answer_macrostate_mfe(energy - other.energy);
	}

	bool operator<=(const answer_macrostate_mfe& other) const {
		assert(!empty_);
		assert(!other.empty_);
		return energy <= other.energy;
	}
};

inline std::ostream &operator<<(std::ostream &o, const answer_macrostate_mfe &tuple) {
	o << '(' << tuple.energy << ", " << tuple.firstStem << ", " << tuple.lastStem << ')';
	return o;
}

inline void empty(answer_macrostate_mfe &e) {
	e.empty_ = true;
}

inline bool isEmpty(const answer_macrostate_mfe &e) {
	return e.empty_;
}


/*
   With the MacroState grammar there are some ambiguous situations where it is not quite clear how a single unpaired base might dangle. Since we analyse the whole searchspace, we have to account for all candidates. The most complex situation arises for "mladlr", where two unpaired bases X and Y inside of a multiloop might separatly dangle to the closing stem (from inside) or to the first (or last) enclosed stem. Problem is, that we don't know the pairing partner of those enclosed stems, we only know one base of the pair (first 5' base for leftmost stem = F, last 3' base for rightmost stem = T). But the other can be determined by basepairing rules, hindered by additional wobble pairs. Thus we have the four possibilities:
    q1) F forms a Watson Crick Pair (WC), T forms a WC
    q2) F froms WS, T forms a wobble pair (WOB)
    q3) F forms WOB, T forms WC
    q4) F forms WOB, T forms WOB
   This split of the partition function for the search space must be revoked on a later operation.
   At some other situations a similar trick is applied, but for less possibilities; thus the qx don't always mean the same!
*/
struct pftuple{
  double q1;
  double q2;
  double q3;
  double q4;

  pftuple() : q1(0.0), q2(0.0), q3(0.0), q4(0.0) {
  }

  pftuple(double a, double b, double c, double d) : q1(a), q2(b), q3(c), q4(d) {
  }

  pftuple& operator+=(const pftuple &a) {
    q1 += a.q1;
    q2 += a.q2;
    q3 += a.q3;
    q4 += a.q4;
    return *this;
  }
};

inline std::ostream &operator<<(std::ostream &s, const pftuple &pf) {
  s << "(" << pf.q1 << ", " << pf.q2 << ", " << pf.q3 << ", " << pf.q4 << ")";
  return s;
}

#include "rtlib/string.hh"
struct answer_macrostate_pfunc {
  bool empty_;
  Subsequence firststem; //position of the leftmost stem in according sequence
  pftuple pf; //partition function answer tuple

  answer_macrostate_pfunc() : empty_(false) {
  }

  answer_macrostate_pfunc(int i) : empty_(false) {
  }

  answer_macrostate_pfunc& operator+=(const answer_macrostate_pfunc &a) {
    firststem = a.firststem;
    pf += a.pf;
    return *this;
  }
};

inline std::ostream &operator<<(std::ostream &s, const answer_macrostate_pfunc &pfa) {
  if (pfa.empty_) {
    s << 'E';
  } else {
    s << pfa.pf.q1;
  }
  return s;
}

inline void empty(answer_macrostate_pfunc &e) {
  e.empty_ = true;
}

inline bool isEmpty(const answer_macrostate_pfunc &e) {
  return e.empty_;
}

/*
   returns the Watson-Crick pairing partner of base b, i.e. wc_comp(A)=U, wc_comp(C)=G, wc_comp(G)=C and wc_comp(U)=A
*/
inline base_t wc_comp(base_t b) {
  switch (b) {
    case A_BASE:
      return U_BASE;
    case C_BASE:
      return G_BASE;
    case G_BASE:
      return C_BASE;
    case U_BASE:
      return A_BASE;
    default:
      return N_BASE;
  }
}

/*
   returns the wobble base pairing partner of base b, i.e. wc_comp(A)=U, wc_comp(C)=G, wc_comp(G)=U and wc_comp(U)=G
*/
inline base_t wob_comp(base_t b) {
  switch (b) {
    case A_BASE:
      return U_BASE;
    case C_BASE:
      return G_BASE;
    case G_BASE:
      return U_BASE;
    case U_BASE:
      return G_BASE;
    default:
      return N_BASE;
  }
}

/*
   returns the sum of all partition function components
*/
inline double sum_elems(const pftuple &pf) {
  return pf.q1+pf.q2+pf.q3+pf.q4;
}

/*
   Copes with the ambiguous situation "ambd" where a single unpaired base either dangles to an adjacent stem to the left XOR to the right. Unfortunately, we don't know the opening basepair partner of the left stem. We can infere it via basepair rules, hindered by the wobble pairs GU and UG. This we have two situations:
    a) the basal basepair for the left stem is a Watson-Crick pair (AU, UA, CG, GC) or
    b) it is a wobble pair (AU,UA,UG,GU)
   "check_tuple" assures that all parts of the partition function are combined in the correct way (a or b).
*/
inline double check_tuple(double qleft, const Subsequence &firststemLeft, const Subsequence &firststemRight, const Subsequence &ambiguousBase, const pftuple &qright) {
  return qleft * (qright.q1 + qright.q2) * mk_pf(min(dr_energy(firststemLeft,firststemLeft), dl_dangle_dg(base_t(ambiguousBase[ambiguousBase.i]), base_t(firststemRight[firststemRight.i]),  wc_comp(base_t(firststemRight[firststemRight.i]))))) +
         qleft * (qright.q3 + qright.q4) * mk_pf(min(dr_energy(firststemLeft,firststemLeft), dl_dangle_dg(base_t(ambiguousBase[ambiguousBase.i]), base_t(firststemRight[firststemRight.i]), wob_comp(base_t(firststemRight[firststemRight.i])))));
}

/*
   multiplies x component wise to partition function tuple pf
*/
inline pftuple mult_tup(double x, const pftuple &pf) {
  return pftuple(
    pf.q1 * x,
    pf.q2 * x,
    pf.q3 * x,
    pf.q4 * x
  );
}

/*
   pushes x either to q1 or to q4, depending on the type of the basal basepair of stem. x goes to q1 if it is a Watson-Crick basepair, otherwise x is in q4
*/
inline pftuple mk_tuple(const Subsequence &stem, double x) {
  pftuple res;

  if ((base_t(stem[stem.i]) == G_BASE && base_t(stem[stem.j-1]) == U_BASE)
   || (base_t(stem[stem.i]) == U_BASE && base_t(stem[stem.j-1]) == G_BASE)) {
    res.q4 = x;
  } else {
    res.q1 = x;
  }

  return res;
}


/*
   necessary for stochastical backtracing, aka sampling. Because the probabilities are sometimes split over four components, we have to add them all up, when sampling.
*/
#ifdef USE_GSL

#include "sample.hh"
// fuehrt das hier nicht zu falschen Ergebnissen, wenn nur nach q1 gefragt wird??
struct PfanswerToDouble {
  double operator()(const answer_macrostate_pfunc &pf) const {
    return pf.pf.q1;
  }
};

template<typename S, typename T, typename pos_int>
inline List_Ref<std::pair<S, T>, pos_int>
sample_filter_pf(List_Ref<std::pair<S, T>, pos_int> &x) {
  return sample_filter(x, PfanswerToDouble());
}

//only used in sample_filter_pf_all, see below
struct PfanswerToDoubleAll {
  double operator()(const answer_macrostate_pfunc &pf) const {
    return pf.pf.q1 + pf.pf.q2 + pf.pf.q3 + pf.pf.q4;
  }
};

// used in macrostate.gap, e.g. for: "instance pfsampleshape5all = gra_macrostate ( ( (alg_pfunc_macrostate | alg_pfunc_macrostate_id ) * alg_shape5 ) suchthat sample_filter_pf_all ) ; //compile with --sample !"
template<typename S, typename T, typename pos_int>
inline List_Ref<std::pair<S, T>, pos_int> sample_filter_pf_all(List_Ref<std::pair<S, T>, pos_int> &x) {
  return sample_filter(x, PfanswerToDoubleAll());
}

#endif

#endif
