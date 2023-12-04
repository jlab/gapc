
#include <iostream>

struct A {
  A()
  {
    std::cerr << "Cons A" << std::endl;
  }
  ~A()
  {
    std::cerr << "Des A" << std::endl;
  }
  int i;
};


struct B : public A {
  B()
  {
    std::cerr << "Cons B" << std::endl;
  }
  ~B()
  {
    std::cerr << "Des B" << std::endl;
  }
};

void x(A *a)
{
  delete a;
}

void y(B *b)
{
  delete b;
}

int main()
{
  B *b = new B();
  x(b);
  B *bb = new B();
  y(bb);
  return 0;
}
