#include <iostream>

template<typename T>
struct Left_Return
{
  typedef T type;
};


template<typename A, typename B>
struct Left_Return<std::pair<A, B> >
{
  typedef typename Left_Return<typename std::pair<A, B>::first_type>::type type;
};

template<class T>
inline T & left_most(T &e)
{
  return e;
}

template<typename A, typename B>
inline typename Left_Return<std::pair<A, B> >::type  & left_most(std::pair<A, B> &x)
{
  return left_most(x.first);
}


int main(int argc, char **argv)
{
  std::pair<int, int> p(23, 42);
  int i;
  i = left_most(p);
  std::cerr << i << '\n';

  std::pair<std::pair<int, int>, int> q;
  q.first.first = 1;
  q.first.second = 2;
  q.second = 3;
  i = left_most(q);
  std::cerr << i << '\n';
 
  return 0;
}
