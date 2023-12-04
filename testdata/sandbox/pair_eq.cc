#include <iostream>

int main(int argc, char **argv)
{
  std::pair<int, int> p(1,2),
    q(2,3),
    r(1,2);
  std::cout << (p == q) << std::endl;
  std::cout << (p == r) << std::endl;
  std::pair<std::pair<int, int>, std::pair<int,int> > a, b, c;
  a.first.first = 1;
  a.first.second = 2;
  a.second.first = 3;
  a.second.second = 4;
  b.first.first = 2;
  b.first.second = 2;
  b.second.first = 3;
  b.second.second = 4;
  c.first.first = 1;
  c.first.second = 2;
  c.second.first = 3;
  c.second.second = 4;
  std::cout << (a == c) << std::endl;
  std::cout << (a == b) << std::endl;
  return 0;
}
