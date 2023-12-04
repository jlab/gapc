#include <iostream>

typedef 
struct { int a; int b; } foo;

void bar(foo x)
{
  std::cerr << x.a << ' ' << x.b << std::endl;
}


int main(int argc, char **argv)
{
  foo a;
  a.a = 1;
  a.b = 2;
  bar(a);
  return 0;
}
