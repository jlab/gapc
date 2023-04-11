#ifndef EXT_HMM_HH
#define EXT_HMM_HH

template <typename T>
inline
T negexpsum(T t) {
  return t;
}

template <typename Itr>
inline
typename std::iterator_traits<Itr>::value_type negexpsum(Itr begin, Itr end) {
  typename std::iterator_traits<Itr>::value_type n;
  if (begin == end) {
    empty(n);
    return n;
  }
  assert(!isEmpty(*begin));
  n = exp(-1.0 * *begin);
  ++begin;
  for (; begin != end; ++begin) {
    assert(!isEmpty(*begin));
    n += exp(-1.0 * *begin);
  }
  assert((n > 0 && "Your algebra produces (partial) candidates with negative "
                   "score, which cannot be logarithmized. Avoid h=negexpsum or "
                   "ensure all positive values!"));
  return -1.0 * log(n);
}

template <typename Iterator>
inline
typename std::iterator_traits<Iterator>::value_type
negexpsum(std::pair<Iterator, Iterator> &p) {
  return negexpsum(p.first, p.second);
}

const static double THRESHOLD = 0.000000001;
inline List_Ref<std::pair<Rope, double> > filterLowProbLabels(List_Ref<std::pair<Rope, double> > candidates_orig) {
	//return candidates_orig;
  // do nothing if there is just one candidate at all
  if (candidates_orig.ref().size() <= 1) {
	return candidates_orig;
  }

  List_Ref<std::pair<Rope, double> > candidates;
  // obtain most probable candidate up to here
  double bestValue = (*candidates_orig.ref().begin()).second;
  for (List_Ref<std::pair<Rope, double> >::iterator i = candidates_orig.ref().begin(); i != candidates_orig.ref().end(); ++i) {
	  if (bestValue > (*i).second) {
		  bestValue = (*i).second;
	  }
  }

  // only keep candidates that are not smaller than the best candidate times THRESHOLD
  // a value that should be changable by the user instead of hard coding it here
  for (List_Ref<std::pair<Rope, double> >::iterator i = candidates_orig.ref().begin(); i != candidates_orig.ref().end(); ++i) {
	  if ((*i).second <= (bestValue + log(1/THRESHOLD))) {
	    candidates.ref().push_back(*i);
	  }
  }
  return candidates;
}

//template <typename T>
//struct filterLowProbLabels2 {
//  double bestValue;
//  filterLowProbLabels2() : bestValue(0.0) {
//  }
//  void update(const T &src) {
//	if (src.second > bestValue) {
//		bestValue = src.second;
//	}
//  }
//  bool ok(const T &x) const {
//	  return x.second <= lowProbabilityFilter();
////	  //sum = sum + x.second;
////	  //return true;
////	  double thresh = pow(22.56 * lowProbabilityFilter(), (x.first.size()-1));
////    //double thresh = 0.1 * sum;
//////	  if (sum > 0) {
////		  std::cerr << sum << ", " << x.second <<  "\t" << x.first << "\n";
//////	  }
//////	  if (x.second != 1.0) {
//////	    std::cerr << (thresh > x.second) << "\t" << thresh << "\t" << x.second << "\n";
//////	  }
////    //std::cerr << (pow(0.01, x.first.size())) << " " << x.second << "\n"; // "thresh: " << x.second << ", sum: " << sum << "\n";
////    //return (sum > 0.01) || (count <= 1);
////	  return true; //x.second > 0.3;
////	  return (x.second > thresh) || (x.second == 1.0);
////	  //return x.second > 0.5;
//  }
//};
//
//template <typename SHAPE>
//inline double getPfuncValue(std::pair<SHAPE, answer_macrostate_pfunc> x) {
//	return sum_elems(x.second.pf);
//}



#endif
