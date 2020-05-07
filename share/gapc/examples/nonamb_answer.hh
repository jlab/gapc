
#ifndef NONAMB_ANSWER_HH
#define NONAMB_ANSWER_HH

	struct pftuple{
		double q1;
		double q2;
		double q3;
		double q4;
		
		pftuple()
                  : 
			q1(0.0),
			q2(0.0),
			q3(0.0),
			q4(0.0)
                {
		}

		pftuple(double a, double b, double c, double d)
                  :
			q1(a),
			q2(b),
			q3(c),
			q4(d)
                {
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
	struct pfanswer{
		bool empty_;
		Subsequence firststem;
		Subsequence subword;
		pftuple pf;
		
		pfanswer() : empty_(false) {
		}
		
		pfanswer(int i) : empty_(false) {
		}
		
		pfanswer& operator+=(const pfanswer &a) {
			subword = a.subword;
			firststem = a.firststem;
			pf += a.pf;
			return *this;
		}
	};
	inline std::ostream &operator<<(std::ostream &s, const pfanswer &pfa) {
		//s << "(firststem: " << pfa.firststem << ", subword: " << pfa.subword << ", pf: "  << pfa.pf << ")";
                if (pfa.empty_)
                  s << 'E';
                else
                  s << pfa.pf.q1;
		return s;
	}
	inline void empty(pfanswer &e) {e.empty_ = true; }
	inline bool isEmpty(const pfanswer &e) { return e.empty_; }
	
inline base_t wc_comp(base_t b)
{
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

inline base_t wob_comp(base_t b)
{
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
	
inline double sum_elems(const pftuple &pf)
{
  return pf.q1+pf.q2+pf.q3+pf.q4;
}

	inline double check_tuple(double qleft, const Subsequence &firststemLeft, const Subsequence &firststemRight, const Subsequence &ambiguousBase, const pftuple &qright) {
		return qleft * (qright.q1 + qright.q2) * mk_pf(min(dr_energy(firststemLeft,firststemLeft), dl_dangle_dg(base_t(ambiguousBase[ambiguousBase.i]), base_t(firststemRight[firststemRight.i]),  wc_comp(base_t(firststemRight[firststemRight.i]))))) +
		       qleft * (qright.q3 + qright.q4) * mk_pf(min(dr_energy(firststemLeft,firststemLeft), dl_dangle_dg(base_t(ambiguousBase[ambiguousBase.i]), base_t(firststemRight[firststemRight.i]), wob_comp(base_t(firststemRight[firststemRight.i])))));
	}
	
inline pftuple mult_tup(double x, const pftuple &pf)
{
  return pftuple(
      pf.q1 * x,
      pf.q2 * x,
      pf.q3 * x,
      pf.q4 * x
      );
}

inline pftuple mk_tuple(const Subsequence &stem, double x)
{
  pftuple res;

  if ((base_t(stem[stem.i]) == G_BASE && base_t(stem[stem.j-1]) == U_BASE)
      || (base_t(stem[stem.i]) == U_BASE && base_t(stem[stem.j-1]) == G_BASE)) {
    res.q4 = x;
  } else {
    res.q1 = x;
  }

  return res;
}


#ifdef USE_GSL

#include "sample.hh"

struct PfanswerToDouble
{
  double operator()(const pfanswer &pf) const
  {
    return pf.pf.q1;
  }
};

template<typename S, typename T, typename pos_int>
inline
List_Ref<std::pair<S, T>, pos_int>
sample_filter_pf(List_Ref<std::pair<S, T>, pos_int> &x)
{
  return sample_filter(x, PfanswerToDouble());
}

struct PfanswerToDoubleAll
{
  double operator()(const pfanswer &pf) const
  {
    return pf.pf.q1 + pf.pf.q2 + pf.pf.q3 + pf.pf.q4;
  }
};

template<typename S, typename T, typename pos_int>
inline
List_Ref<std::pair<S, T>, pos_int>
sample_filter_pf_all(List_Ref<std::pair<S, T>, pos_int> &x)
{
  return sample_filter(x, PfanswerToDoubleAll());
}

#endif

#endif

