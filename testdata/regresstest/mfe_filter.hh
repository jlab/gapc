#ifndef MFE_FILTER_HH
#define MFE_FILTER_HH


struct Mfe_Filter {

  static int thresh;

  static bool ok(int mfe)
  {
    return mfe <= thresh;
  }
};

#ifdef GAPC_MOD_TRANSLATION_UNIT

int Mfe_Filter::thresh = 0;

#endif

inline bool in_mfe_thresh(int t)
{
  return Mfe_Filter().ok(t);
}

template <typename T>
inline bool in_mfe_thresh(const std::pair<int, T> &t)
{
  return Mfe_Filter().ok(t.first);
}

#endif
