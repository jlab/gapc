#include <gmpxx.h>

#include <iostream>

#include <cmath>

int main()
{
  mpq_class a(1,4), b(1,2), c(1,4);
  std::cout << a*b*c << std::endl;

  mpq_class z("788671864584357/73786976294838206464");
  std::cout << z << std::endl;

  mpq_class x(1.0688496862006918e-5);
  std::cout << x << std::endl;

  mpq_class y(std::exp(-7.934/log(2)));
  std::cout << y << std::endl;

  mpz_class r(2147483647);
  std::cout << r << std::endl;
  r += 2147483647;
  std::cout << r << std::endl;
  r *= 2147483647;
  std::cout << r << std::endl;
  return 0;
}
