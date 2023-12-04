#include <iostream>

static float e = 101.101;

template<class T, typename pos_int> class List;

template<typename T> inline T empty()
{
  T r = 0;
  return r;
}

template<> inline float* empty<float*>()
{
  return &e;
}


// FIXME rename not_empty to is_not_empty
template<typename T> inline bool is_not_empty(T &e)
{
  return !(e == 0);
}

template<class T, typename pos_int> inline bool
  is_not_empty(List<T, pos_int> *l)
{
  return !l->isEmpty();
}

int main(int argc, char **argv)
{
  int a = 1;
  int *b = &a;
  std::cerr << b << std::endl;
  b = empty<int*>();
  std::cerr << b << std::endl;
  float c = 3.2;
  float *d = &c;
  std::cerr << *d << std::endl;
  d = empty<float*>();
  std::cerr << *d << std::endl;
  return 0;
}
