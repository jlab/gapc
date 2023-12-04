#ifndef EXT_HMM_HH
#define EXT_HMM_HH

#include <utility>

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

static const double THRESHOLD = 0.000000001;
inline List_Ref<std::pair<Rope, double> > filterLowProbLabels(
  List_Ref<std::pair<Rope, double> > candidates_orig) {
  // return candidates_orig;
  // do nothing if there is just one candidate at all
  if (candidates_orig.ref().size() <= 1) {
    return candidates_orig;
  }

  List_Ref<std::pair<Rope, double> > candidates;
  // obtain most probable candidate up to here
  double bestValue = (*candidates_orig.ref().begin()).second;
  for (List_Ref<std::pair<Rope, double> >::iterator i =
    candidates_orig.ref().begin(); i != candidates_orig.ref().end(); ++i) {
    if (bestValue > (*i).second) {
      bestValue = (*i).second;
    }
  }

  /* only keep candidates that are not smaller than the best candidate times
   * THRESHOLD a value that should be changable by the user instead of hard
   * coding it here */
  for (List_Ref<std::pair<Rope, double> >::iterator i =
       candidates_orig.ref().begin(); i != candidates_orig.ref().end(); ++i) {
    if ((*i).second <= (bestValue + log(1/THRESHOLD))) {
      candidates.ref().push_back(*i);
    }
  }
  return candidates;
}

#endif  // EXT_HMM_HH
