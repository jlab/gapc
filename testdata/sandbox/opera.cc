#include <iostream>
#include <cstdio>

//struct A; struct B;


//std::ostream &operator<<(std::ostream &out, const A &a);

////std::ostream &operator<<(std::ostream &out, const B &a);



struct A {
  virtual void x(std::ostream &out) const { puts("A"); }
};


struct B : public A {
  void x(std::ostream &out) const { puts("B"); }
};


std::ostream &operator<<(std::ostream &out, const A &a)
{
  a.x(out);
  return out;
}


template <typename T>
std::ostream &operator<<(std::ostream &out, const T &t)
{
  puts("T");
  return out;
}

int main(int argc, char **argv)
{
  A a;
  B b;
  A *c = &b;
  std::cerr << a << std::endl;
  std::cerr << *c << std::endl;
  std::cerr << b << std::endl;
  return 0;
}

/*
std::ostream &operator<<(std::ostream &out, const B &a);
{
  a.x(out);
  return out;
}
*/

