
#include <boost/pool/object_pool.hpp>

#include <iostream>

struct A {
  A() { std::cerr << "construct A\n"; }
  ~A() { std::cerr << "destuct A\n"; }
};

struct B {
  B() { std::cerr << "construct B\n"; }
  ~B() { std::cerr << "destuct B\n"; }
};

static boost::object_pool<B> pest;
static boost::object_pool<A> pool;

struct Dummy {
  static boost::object_pool<A> pool;
  static boost::object_pool<B> pest;
};

boost::object_pool<A> Dummy::pool;
boost::object_pool<B> Dummy::pest;

int main(int argc, char **argv)
{
  std::cerr << "main\n";
  for (int i = 0; i < 10; i++) {
    std::cerr << i << '\n';
    //pool.malloc();
    //pest.malloc();
    Dummy::pool.malloc();
    Dummy::pest.malloc();
  }
  return 0;
}
