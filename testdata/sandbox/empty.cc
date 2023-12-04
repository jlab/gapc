#include <iostream>

template<typename T> inline bool isEmpty(const T &x)
{
  std::cerr << "T isEmpty\n";
  return x == 0;
}

template<typename T> inline bool is_not_empty(const T &x)
{
  std::cerr << "T is_not_empty\n";
  return !isEmpty(x);
}


struct mfeanswer {
  int energy;
/*  Subsequence leftBase;
  Subsequence rightBase;
  String rep;*/
  bool empty_;
  mfeanswer() : empty_(false) {}
bool operator>(const mfeanswer& other) const { return this->energy > other.energy; }
bool operator<(const mfeanswer& other) const { return this->energy < other.energy; }
bool operator==(const mfeanswer& other) const { return this->energy == other.energy; }

};

inline std::ostream &operator<<(std::ostream &o, const mfeanswer &tuple) {
  o << '(' << tuple.energy
  /*  << ", " << tuple.leftBase
   << ", " << tuple.rightBase
   << ", " << tuple.rep*/
   << ')' ;
  return o;
}

inline void empty(mfeanswer &e) {
  e.empty_ = true;
}
inline bool isEmpty(const mfeanswer &e) {
  std::cerr << "mfeanswer isEmpty\n";
  return e.empty_;
}



int main()
{
  mfeanswer x;
  if (is_not_empty(x))
    std::cerr << "is_not_empty: true\n";
  x.empty_ = true;
  if (is_not_empty(x))
    std::cerr << "is_not_empty: true\n";
  return 0;
}
