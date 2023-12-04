#include <iostream>
#include <cstdlib>

#include <iterator>
#include <list>

int main(int argc, char **argv)
{
  // Sun CCs default STL don't implement standard conform iterator_traits
  // for example
  std::iterator_traits<std::list<int>::iterator>::value_type i = 23;
  std::cout << "Hello World " << i << std::endl;
  std::exit(0);
  return 0;
}
