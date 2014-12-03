#include <iostream>

class A;

class B {
  public:
    A* a;
    B();
    ~B();
};

B::~B() { delete a; }

class A {
  public:
    ~A();
};

A::~A() { std::cerr << "Destruct A: " << this << '\n'; }

B::B() { a = new A(); }

int main()
{
  B b;
  std::cout << "Constructed: " << &b << '\n';
  return 0;
}
