#include "../rtlib/singleton.hh"

#include <iostream>

int main()
{
  Singleton<int> s;
  int &i = s.ref();
  i = 3;
  std::cerr << i << std::endl;
  return 0;
}
