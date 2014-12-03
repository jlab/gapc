
#include <list>
#include <iostream>
#include <boost/foreach.hpp>

int main(int argc, char **argv)
{
  std::list<int> l;
  for (int i = 0; i<50; ++i)
    l.push_back(i);

  BOOST_FOREACH(int i, l) {
    std::cout << i << std::endl;
  }
  return 0;
}
