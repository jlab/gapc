#include <iostream>

struct pair { int a; int b; };

pair bar(int a) {
  pair p;
  p.a = a * a * a;
  p.b = a * a;
  return p;
}

const pair foo(int a) {
  const pair &r = bar(a);
  // r could be destructed after the previous line ...
  return r;
}

int main(int argc, char **argv)
{
  std::cout << foo(argc).a << std::endl;
  return 0;
}
