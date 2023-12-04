
#include <string>
#include <iostream>

int main(int argc, char **argv)
{
  std::list<int> l;
  for (int i = 0; i<50; ++)
    l.push_back(i);

  for (std::list<int>::iterator i = l.begin();
      i != l.end(); ++i) {
    std::cout << *i << std::endl;
  }
  return 0;
}
