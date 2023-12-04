
#include <stdlib.h>

#include <iostream>

int main(int argc, char **argv)
{
  char *x = (char*) malloc(size_t(atoi(argv[1]))*size_t(1024)*size_t(1024));
  x[0] = 'H';
  std::cerr << atoi(argv[1]) << " " << x[0] << "ello\n";
  return 0;
}
