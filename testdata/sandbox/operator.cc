#include <iostream>


using namespace std;


class A {
  public:
    int i;
    A(int a) : i(a) {}

    A & operator+=(const A& a) { i += a.i; return *this; }
    bool operator==(const A& a) const { return i == a.i; }
};

class B : public A {
  public:
    int p;
    B(int a) : A(a+a), p(a) {}
};


std::ostream &operator<<(std::ostream &s, const A &a)
{
  s << a.i;
  return s;
}

A * operator+(A &a, const A &b) { return &a; } 



int main(int argc, char **argv)
{
  A a(42);
  B b(23);
  cout << a << endl;
  a += b;
  cout << a << endl;
  cout << (b == a) << endl;
  cout << b << endl;
  A *x = a + a;
  cout << *x << endl;
  return 0;
}
