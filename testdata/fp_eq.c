#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <errno.h>
#include <stdio.h>

int main(int argc, char **argv)
{
  assert(argc == 4);
  errno = 0;
  double a = strtod(argv[1], 0);
  if (errno)
    return 23;
  errno = 0;
  double b = strtod(argv[2], 0);
  if (errno)
    return 42;

  errno = 0;
  double eps = strtod(argv[3], 0);
  if (errno)
    return 66;

  printf("%g %g %g %g %s\n", a, b, fabs(a-b), eps, fabs(a-b) > eps ? "false" : "true");

  return fabs(a-b) > eps;

}
