
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <assert.h>

int main(int argc, char **argv)
{
  size_t x = sizeof(int);
  size_t n = atoi(argv[1]);
  FILE *fd = fopen("/dev/urandom", "r");
  for (size_t i = 0; i<n; ++i) {
    char buffer[x];
    size_t r = fread(buffer, x, 1, fd);
    assert(r == 1);
    printf("%u\n", abs(*((int*)buffer)));
  }
  return 0;
}
