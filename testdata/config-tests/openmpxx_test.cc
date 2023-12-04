#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

static void read(const char *fn, std::vector<double> &x)
{
  std::ifstream f(fn);
  for (;;) {
    double d;
    f >> d;
    if (!f.good())
      break;
    x.push_back(d);
  }
}


int main(int argc, char **argv) {
  std::vector<double> a, b;
  double c = 0;
  #pragma omp parallel
  {
    #pragma omp sections
    {
      #pragma omp section
      read(argv[1], a);
      #pragma omp section
      read(argv[2], b);
    }
    #pragma omp for reduction(+:c)
    for (int i = 0; i < int(a.size()); ++i)
      c += a[i]*b[i];
  }
  std::cout << c << '\n'; return 0;
}
