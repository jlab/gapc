#include <iostream>
#include <limits>

int main(int argc, char **argv)
{
  int i = atoi(argv[1]);


  //char s[10];
  char s[11];
  s[10] = 0;
  char *c = s+9;
  *c = '0';
  c++;
  if (!i)
    c--;
  while (i) {
    c--;
    int a = i % 10;
    std::cerr << a << std::endl;
    *c = '0' + i % 10;
    i /= 10;
  }
  
  std::cerr << 
  std::numeric_limits<int>::max()
  << std::endl;

  std::cerr << c << std::endl;

  std::cerr << "length " << (10 - (c-s)) << std::endl;

  return 0;
}
