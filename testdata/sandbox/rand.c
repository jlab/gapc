
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
  size_t x = sizeof(long int);
  size_t n = atoi(argv[1]);
  FILE *fd = fopen("/dev/urandom", "r");
  for (size_t i = 0; i<n; ++i) {
    char buffer[x];
    size_t r = fread(buffer, x, 1, fd);
    printf("%lu\n", *((unsigned long int*)buffer));
  }
  return 0;
}
