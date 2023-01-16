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

#endif
