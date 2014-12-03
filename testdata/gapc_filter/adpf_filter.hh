#ifndef ADPF_FILTER_HH
#define ADPF_FILTER_HH


template <typename T>
struct p_func_filter
{
  double sum;
  p_func_filter()
    : sum(0) {}
  void update(const T &src)
  {
    sum += src.second;
  }
  bool ok(const T &x) const
  {
    double thresh = 0.000001 * sum;
    return x.second > thresh;
  }
};

// cart*  --backtrack
inline bool operator==(const std::pair<int, BigInt> &a,
                       const std::pair<int, BigInt> &b)
{
  return a.first == b.first;
}

#endif
