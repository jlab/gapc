#include "rnalib.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void conv(char *x)
{
// @ -> 0, A -> 1, C -> 2, G -> 3, U -> 4
  int l = strlen(x);
  for (int i = 0; i < l; i++)
    switch (x[i]) {
      case 'a' : x[i] = 1; break;
      case 'c' : x[i] = 2; break;
      case 'g' : x[i] = 3; break;
      case 'u' : x[i] = 4; break;
      default: exit(1);
    };
}

int main(int argc, char **argv)
{

  int a = atoi(argv[1]);
  int i = atoi(argv[2]);
  int j = atoi(argv[3]);
  char *inp = argv[4];
  int n = strlen(inp);
  int r = 0;
  int k = 0;
  int l = 0;
  if (argc > 5) {
    k = atoi(argv[5]);
    l = atoi(argv[6]);
  }

  conv(inp);

  switch (a) {
    case 0 : r = dl_energy(inp, i, j);
             break;
    case 1 : r = sr_energy(inp, i, j);
             break;
    case 11 : r = dr_energy(inp, i, j, n);
             break;
    default: return 1;
  };
  printf("%d\n", r);
  return 0;
}
